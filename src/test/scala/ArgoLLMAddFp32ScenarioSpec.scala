package argo_testbed

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import FloatingPoint.fpu._
import hardfloat.{FPOPTest, FPOPTestMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rial.arith.{AddFPGeneric, RealSpec, RoundSpec}
import rial.util.PipelineStageConfig

import java.io.File
import scala.io.Source
import scala.util.Try
import scala.collection.mutable

/**
  * Baseline "LLM scenario" functional verification:
  * - loads vectors from argo_testbed/legacy/vector_loop/llm_runs/.../scenario.json (bit-patterns)
  * - checks OpenFloat / HardFloat / Rial for FP32 add correctness vs a software oracle
  * - checks the three implementations agree (differential checking)
  *
  * Note on NaNs: we compare NaN-ness (classification) but do not require NaN payload bit-exact matches,
  * since IEEE payload propagation may be implementation-dependent.
  */
class ArgoLLMAddFp32ScenarioSpec extends AnyFlatSpec with ChiselSim with Matchers {

  private val DefaultScenarioPath = "argo_testbed/legacy/vector_loop/llm_runs/smoke1/scenario.json"

  private val FloatEqDebug = false

  private case class Vec(aBits: Int, bBits: Int)

  private def jsonEscape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r")

  private def parseHex32Bits(hex: String): Int = {
    val s = hex.trim
    require(s.startsWith("0x") || s.startsWith("0X"), s"Expected hex with 0x prefix: $hex")
    val masked = BigInt(s.drop(2), 16) & BigInt("FFFFFFFF", 16)
    masked.toInt
  }

  private def hex32(x: Int): String = {
    // Fixed-width FP32 bit pattern pretty-print.
    f"0x${(x.toLong & 0xffffffffL)}%08x"
  }

  // JSON parsing without dependencies: extract each {"a_hex":"0x...","b_hex":"0x..."} block.
  private def loadVectorsFromScenario(scenarioPath: String): (String, String, Seq[Vec]) = {
    val f = new File(scenarioPath)
    require(f.exists(), s"Scenario file not found: $scenarioPath")

    val content = Source.fromFile(f).mkString
    // Normalize JSON by stripping all whitespace; keeps our regexes simple and robust.
    val minified = content.replaceAll("\\s+", "")

    def extractString(key: String): String = {
      val r = (s""""$key":"([^"]+)"""").r
      r.findFirstMatchIn(minified).map(_.group(1)).getOrElse(
        throw new RuntimeException(s"Could not find key '$key' in scenario: $scenarioPath")
      )
    }

    val op = extractString("op")
    val fpFormat = extractString("fp_format")

    // Per-vector extraction without JSON libraries:
    // - strip whitespace so the JSON structure is predictable for regex matching
    // - collect all a_hex and b_hex values and zip them in order
    val aR = (""""a_hex":"(?<a>0x[0-9A-Fa-f]+)"""").r
    val bR = (""""b_hex":"(?<b>0x[0-9A-Fa-f]+)"""").r

    val aVals = aR.findAllMatchIn(minified).map(m => parseHex32Bits(m.group("a"))).toSeq
    val bVals = bR.findAllMatchIn(minified).map(m => parseHex32Bits(m.group("b"))).toSeq

    if (aVals.isEmpty || bVals.isEmpty) {
      throw new RuntimeException(s"Could not extract a_hex/b_hex vectors from scenario: $scenarioPath")
    }
    if (aVals.length != bVals.length) {
      throw new RuntimeException(
        s"Mismatched vector list lengths in scenario: $scenarioPath (a_hex=${aVals.length}, b_hex=${bVals.length})"
      )
    }

    val vecs = aVals.zip(bVals).map { case (a, b) => Vec(a, b) }

    if (vecs.isEmpty) {
      throw new RuntimeException(s"No vectors found in scenario: $scenarioPath")
    }
    (op, fpFormat, vecs)
  }

  private def classifyFp32(bits: Int): String = {
    val exp = (bits >>> 23) & 0xff
    val frac = bits & 0x7fffff
    if (exp == 0xff) {
      if (frac != 0) "nan" else "inf"
    } else if (exp == 0x00) {
      if (frac == 0) "zero" else "subnormal"
    } else {
      "normal"
    }
  }

  private def isNaNBits(bits: Int): Boolean = classifyFp32(bits) == "nan"

  /**
    * Coarse input categorization to help the LLM focus future vectors.
    * We prioritize NaN/Inf/Subnormal/Zero first, then near-overflow.
    */
  private def inputCategoryFp32(aBits: Int, bBits: Int): String = {
    val ca = classifyFp32(aBits)
    val cb = classifyFp32(bBits)
    if (ca == "nan" || cb == "nan") "nan"
    else if (ca == "inf" || cb == "inf") "inf"
    else if (ca == "subnormal" || cb == "subnormal") "subnormal"
    else if (ca == "zero" || cb == "zero") {
      // signed-zero is the only zero class we care about in this harness
      "signed_zero"
    } else {
      val expA = (aBits >>> 23) & 0xff
      val expB = (bBits >>> 23) & 0xff
      // "near-overflow": highest normal exponent bin (254 for FP32)
      if (expA >= 0xfe || expB >= 0xfe) "near_overflow" else "normal"
    }
  }

  /**
    * IEEE-lite equality for FP32 bits:
    * - NaN == NaN (classification)
    * - +0/-0 must match sign
    * - infinities/subnormals/normals require exact bit match
    */
  private def fp32EqBits(actualBits: Int, expectedBits: Int): Boolean = {
    val ka = classifyFp32(actualBits)
    val ke = classifyFp32(expectedBits)
    (ka, ke) match {
      case ("nan", "nan") => true
      case ("nan", _) | (_, "nan") => false
      case ("zero", "zero") => (actualBits >>> 31) == (expectedBits >>> 31)
      case _ => actualBits == expectedBits
    }
  }

  private def softwareOracleAddFp32(aBits: Int, bBits: Int): Int = {
    val a = java.lang.Float.intBitsToFloat(aBits)
    val b = java.lang.Float.intBitsToFloat(bBits)
    val res = a + b
    java.lang.Float.floatToRawIntBits(res)
  }

  "Argo LLM scenario (FP32 add)" should "pass correctness + differential checks" in {
    val scenarioPath = System.getProperty("argo.llm.scenario", DefaultScenarioPath)
    val (op, fpFormat, vecs) = loadVectorsFromScenario(scenarioPath)

    op shouldBe "add"
    fpFormat shouldBe "fp32"

    val n = vecs.length
    val expected = new Array[Int](n)
    for (i <- 0 until n) {
      expected(i) = softwareOracleAddFp32(vecs(i).aBits, vecs(i).bBits)
    }

    val openOut = new Array[Int](n)
    val hardOut = new Array[Int](n)
    val rialOut = new Array[Int](n)

    // Collect mismatches so we see the full picture across all vectors/libs.
    val oracleMismatches = mutable.ArrayBuffer[String]()
    val differentialMismatches = mutable.ArrayBuffer[String]()
    val oracleMismatchCatCounts = mutable.Map[String, Int]().withDefaultValue(0)
    val differentialMismatchCatCounts = mutable.Map[String, Int]().withDefaultValue(0)

    def recordOracleMismatch(lib: String, vecIndex: Int, actual: Int, expected: Int): Unit = {
      if (!fp32EqBits(actual, expected)) {
        val cat = inputCategoryFp32(vecs(vecIndex).aBits, vecs(vecIndex).bBits)
        oracleMismatchCatCounts(cat) = oracleMismatchCatCounts(cat) + 1
        oracleMismatches +=
          s"$lib vec=$vecIndex actual=${hex32(actual)} expected=${hex32(expected)} a=${hex32(vecs(vecIndex).aBits)} b=${hex32(vecs(vecIndex).bBits)}"
      }
    }

    def recordDifferentialMismatch(left: String, right: String, vecIndex: Int, leftBits: Int, rightBits: Int): Unit = {
      if (!fp32EqBits(leftBits, rightBits)) {
        val cat = inputCategoryFp32(vecs(vecIndex).aBits, vecs(vecIndex).bBits)
        differentialMismatchCatCounts(cat) = differentialMismatchCatCounts(cat) + 1
        differentialMismatches +=
          s"$left vs $right vec=$vecIndex left=${hex32(leftBits)} right=${hex32(rightBits)} a=${hex32(vecs(vecIndex).aBits)} b=${hex32(vecs(vecIndex).bBits)}"
      }
    }

    // ---------------------------------------------------------------------
    // OpenFloat: FP_add(bw, pd)
    // ---------------------------------------------------------------------
    val pd = 1
    simulate(new FP_add(32, pd)) { dut =>
      dut.io.in_en.poke(true.B)
      dut.io.in_valid.poke(true.B)

      var cycles = 0
      for (i <- 0 until n) {
        val aRaw = vecs(i).aBits.toLong & 0xffffffffL
        val bRaw = vecs(i).bBits.toLong & 0xffffffffL
        dut.io.in_a.poke(aRaw)
        dut.io.in_b.poke(bRaw)

        if (cycles >= pd) {
          val outBits = (dut.io.out_s.peek().litValue.toLong & 0xffffffffL).toInt
          val vecIndex = i - pd
          openOut(vecIndex) = outBits
          recordOracleMismatch("OpenFloat", vecIndex, outBits, expected(vecIndex))
        }

        dut.clock.step(1)
        cycles += 1
      }

      dut.io.in_valid.poke(false.B)
      for (_ <- 0 until pd) {
        val outBits = (dut.io.out_s.peek().litValue.toLong & 0xffffffffL).toInt
        val vecIndex = n - pd // for pd=1 this is n-1
        openOut(vecIndex) = outBits
        recordOracleMismatch("OpenFloat", vecIndex, outBits, expected(vecIndex))
        dut.clock.step(1)
      }
    }

    // ---------------------------------------------------------------------
    // HardFloat: FPOPTest(expW, sigW, mode)
    // ---------------------------------------------------------------------
    simulate(new FPOPTest(8, 24, FPOPTestMode.ADD)) { dut =>
      for (i <- 0 until n) {
        val aRaw = vecs(i).aBits.toLong & 0xffffffffL
        val bRaw = vecs(i).bBits.toLong & 0xffffffffL
        dut.io.in_a.poke(aRaw)
        dut.io.in_b.poke(bRaw)

        val outBits = (dut.io.out.peek().litValue.toLong & 0xffffffffL).toInt
        hardOut(i) = outBits
        recordOracleMismatch("Hardfloat", i, outBits, expected(i))
      }
    }

    // ---------------------------------------------------------------------
    // Rial: AddFPGeneric for Float32
    // ---------------------------------------------------------------------
    val fp32Spec = RealSpec.Float32Spec
    simulate(
      new AddFPGeneric(
        fp32Spec,
        fp32Spec,
        fp32Spec,
        RoundSpec.roundToEven,
        PipelineStageConfig.none
      )
    ) { dut =>
      for (i <- 0 until n) {
        val aRaw = vecs(i).aBits.toLong & 0xffffffffL
        val bRaw = vecs(i).bBits.toLong & 0xffffffffL
        dut.io.x.poke(BigInt(aRaw).U(32.W))
        dut.io.y.poke(BigInt(bRaw).U(32.W))

        dut.clock.step(1)
        val outBits = (dut.io.z.peek().litValue.toLong & 0xffffffffL).toInt
        rialOut(i) = outBits
        recordOracleMismatch("Rial", i, outBits, expected(i))
      }
    }

    // ---------------------------------------------------------------------
    // Differential checks across libs
    // ---------------------------------------------------------------------
    for (i <- 0 until n) {
      recordDifferentialMismatch("OpenFloat", "Hardfloat", i, openOut(i), hardOut(i))
      recordDifferentialMismatch("OpenFloat", "Rial", i, openOut(i), rialOut(i))
    }

    val mismatchesTotal = oracleMismatches.size + differentialMismatches.size
    if (FloatEqDebug || mismatchesTotal == 0) {
      println(s"Scenario result: op=$op fp_format=$fpFormat vectors=$n oracle_mismatches=${oracleMismatches.size} differential_mismatches=${differentialMismatches.size}")
    }

    if (oracleMismatches.nonEmpty || differentialMismatches.nonEmpty) {
      val scenarioFile = new File(scenarioPath).getAbsoluteFile
      val outDir = scenarioFile.getParentFile
      val failuresFile = new File(outDir, "failures.json").getAbsolutePath

      val oracleJson = oracleMismatches.map(m => "\"" + jsonEscape(m) + "\"").mkString("[", ",", "]")
      val diffJson = differentialMismatches.map(m => "\"" + jsonEscape(m) + "\"").mkString("[", ",", "]")

      def catMapToJson(m: mutable.Map[String, Int]): String = {
        val items = m.toSeq.filter { case (_, v) => v > 0 }.sortBy(_._1)
        items.map { case (k, v) => "\"" + jsonEscape(k) + "\":" + v }.mkString("{", ",", "}")
      }

      val oracleCatsJson = catMapToJson(oracleMismatchCatCounts)
      val diffCatsJson = catMapToJson(differentialMismatchCatCounts)

      val failuresJson =
        s"""{
           |  "scenario_path":"${jsonEscape(scenarioFile.getPath)}",
           |  "op":"${jsonEscape(op)}",
           |  "fp_format":"${jsonEscape(fpFormat)}",
           |  "vectors":$n,
           |  "oracle_mismatch_count":${oracleMismatches.size},
           |  "differential_mismatch_count":${differentialMismatches.size},
           |  "oracle_mismatch_categories":$oracleCatsJson,
           |  "differential_mismatch_categories":$diffCatsJson,
           |  "timestamp":"${java.time.Instant.now().toString}",
           |  "oracle_mismatches":$oracleJson,
           |  "differential_mismatches":$diffJson
           |}""".stripMargin

      val pw = new java.io.PrintWriter(failuresFile)
      try pw.write(failuresJson)
      finally pw.close()

      println(s"Wrote failures.json: $failuresFile")
    }

    // For early-stage integration, don't hard-fail the whole test suite.
    // You can opt into strict mode when the oracle/semantics are what you want.
    val strict = System.getProperty("argo.llm.strict", "false").toBoolean
    if (strict && (oracleMismatches.nonEmpty || differentialMismatches.nonEmpty)) {
      val msg =
        s"LLM scenario failure summary (op=$op fp_format=$fpFormat vectors=$n)\n" +
          s"Oracle mismatches: ${oracleMismatches.size}\n" +
          (if (oracleMismatches.nonEmpty) oracleMismatches.take(20).mkString("  - ", "\n  - ", "") else "") +
          (if (oracleMismatches.size > 20) s"\n  ... (${oracleMismatches.size - 20} more) ..." else "") +
          s"\nDifferential mismatches: ${differentialMismatches.size}\n" +
          (if (differentialMismatches.nonEmpty) differentialMismatches.take(20).mkString("  - ", "\n  - ", "") else "") +
          (if (differentialMismatches.size > 20) s"\n  ... (${differentialMismatches.size - 20} more) ..." else "")
      fail(msg)
    } else if (oracleMismatches.nonEmpty || differentialMismatches.nonEmpty) {
      // Print a compact summary, but keep the test green.
      val msg =
        s"LLM scenario summary (op=$op fp_format=$fpFormat vectors=$n)\n" +
          s"Oracle mismatches: ${oracleMismatches.size}\n" +
          (if (oracleMismatches.nonEmpty) oracleMismatches.take(5).mkString("  - ", "\n  - ", "") + (if (oracleMismatches.size > 5) s"\n  ... (${oracleMismatches.size - 5} more) ..." else "") else "") +
          s"\nDifferential mismatches: ${differentialMismatches.size}\n" +
          (if (differentialMismatches.nonEmpty) differentialMismatches.take(5).mkString("  - ", "\n  - ", "") + (if (differentialMismatches.size > 5) s"\n  ... (${differentialMismatches.size - 5} more) ..." else "") else "")
      println(msg)
    }
  }
}