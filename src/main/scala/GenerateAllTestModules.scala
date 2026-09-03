package Generate

import FloatingPoint.fpu._
import FloatingPoint.{FP16, FP32, FP64, FP128, FloatingPointFormat}
import hardfloat._
import rial.arith.{AddFPGeneric, MultFPGeneric, FusedMulAddFPGeneric, RealSpec, RoundSpec}
import rial.math.{MathFuncConfig, MathFuncPipelineConfig, MathFunctions, FuncKind}
import rial.util.PipelineStageConfig
import chisel3._
import circt.stage.ChiselStage

import java.io.{File, PrintWriter}
import scala.math.exp

/** Elaborates every design named in generated/elaboration_plan.json
  *
  * Build the plan first:
  *   python3 scripts/build_manifest.py --plan-only
  *
  * Descriptors are the source of truth for which designs exist and how they are parameterized
  * To add a design, add a descriptor YAML to descriptors/, don't edit this file
  * To add a whole library, add a factory below
  */
object GenerateAllTestModules extends App {
  /** Firtool options for Yosys-friendly SystemVerilog.
    * disallowLocalVariables: no automatic logic in always blocks -> fixes "invalid nesting".
    */
  private val yosysFirtoolOpts = Array(
    "--lowering-options=disallowLocalVariables,disallowPackedArrays",
    "--disable-all-randomization",
    "-strip-debug-info",
    "--disable-annotation-unknown"
  )

  private val manifestPath = "generated/elaboration_plan.json"

  private def realSpec(name: String): RealSpec = name match {
    case "FP16" => RealSpec.Float16Spec
    case "FP32" => RealSpec.Float32Spec
    case "FP64" => RealSpec.Float64Spec
    case other  => sys.error(s"unknown Rial spec '$other'")
  }

  private def funcKinds(func: String): Seq[FuncKind.FuncKind] = func match {
    case "Sqrt"       => Seq(FuncKind.Sqrt)
    case "InvSqrt"    => Seq(FuncKind.InvSqrt)
    case "Sin"        => Seq(FuncKind.Sin)
    case "Cos"        => Seq(FuncKind.Cos)
    case "Reciprocal" => Seq(FuncKind.Reciprocal)
    case "Exp"        => Seq(FuncKind.Exp)
    case "Log"        => Seq(FuncKind.Log)
    case "Sigmoid"    => Seq(FuncKind.Sigmoid)
    case "SoftPlus"   => Seq(FuncKind.SoftPlus)
    case "SMG"        => Seq(FuncKind.ScaleMixtureGaussian)
    case "Acos"       => Seq(FuncKind.Sqrt, FuncKind.ACosPhase1, FuncKind.ACosPhase2)
    case "Atan2"      => Seq(FuncKind.Reciprocal, FuncKind.ATan2Phase1, FuncKind.ATan2Phase2)
    case other        => sys.error(s"unknown Rial math function '$other'")
  }

  private def mathConfig(func: String): MathFuncConfig = {
    val kinds = FuncKind.normalize(funcKinds(func))
    // SMG carries the two sigma constants for the table generator
    if (func == "SMG") new MathFuncConfig(kinds, Some((exp(-1.0), exp(-6.0))))
    else new MathFuncConfig(kinds)
  }

  private def hardfloatMode(m: String): FPOPTestMode.Mode = m match {
    case "ADD" => FPOPTestMode.ADD
    case "SUB" => FPOPTestMode.SUB
    case "MUL" => FPOPTestMode.MUL
    case other => sys.error(s"unknown hardfloat op mode '$other'")
  }

  // One entry per module family, keyed by the descriptor's "generator" field
  // E.g. the following descriptor snippet would map to build
  // generator:
  // factory: hardfloat.op
  // params:
  //   expW: 11
  //   sigW: 53
  //   mode: MUL
  // OpenFloat takes a custom FloatingPointFormat type...
  private def openfloatFormat(bw: Int): FloatingPointFormat = bw match {
    case 16  => FP16
    case 32  => FP32
    case 64  => FP64
    case 128 => FP128
    case other => sys.error(s"no OpenFloat format for bw=$other")
  }

  private def build(factory: String, p: ujson.Obj): RawModule = {
    def i(k: String): Int    = p(k).num.toInt
    def s(k: String): String = p(k).str

    factory match {
      case "openfloat.add"  => new FP_add(openfloatFormat(i("bw")), i("pd"))
      case "openfloat.mult" => new FP_mult(openfloatFormat(i("bw")), i("pd"))
      case "openfloat.div"  => new FP_div(openfloatFormat(i("bw")), i("L"), i("latency"))
      case "openfloat.sqrt" => new FP_sqrt(openfloatFormat(i("bw")), i("L"), i("latency"))
      case "openfloat.cos"  => new FP_cos(openfloatFormat(i("bw")), i("iters"))

      case "hardfloat.recfn_test" => new FPTest(i("expW"), i("sigW"))
      case "hardfloat.op"         => new FPOPTest(i("expW"), i("sigW"), hardfloatMode(s("mode")))
      case "hardfloat.div"        => new FPDIVTest(i("expW"), i("sigW"))
      case "hardfloat.sqrt"       => new FPSqrtTest(i("expW"), i("sigW"))

      case "rial.add" =>
        val sp = realSpec(s("spec"))
        new AddFPGeneric(sp, sp, sp, RoundSpec.roundToEven, PipelineStageConfig.none)
      case "rial.mul" =>
        val sp = realSpec(s("spec"))
        new MultFPGeneric(sp, sp, sp, RoundSpec.roundToEven, PipelineStageConfig.none)
      case "rial.fma" =>
        val sp = realSpec(s("spec"))
        new FusedMulAddFPGeneric(sp, sp, sp, sp, RoundSpec.roundToEven, PipelineStageConfig.none)
      case "rial.mathfunc" =>
        new MathFunctions(mathConfig(s("func")), realSpec(s("spec")),
                          i("nOrder"), i("adrW"), i("extraBits"), MathFuncPipelineConfig.none)

      case other => sys.error(s"no factory registered for '$other'")
    }
  }

  // io.sel values keyed by phase name, since multi-phase functions need one per phase
  // Chisel must resolve these so they ride along with the generated RTL
  private def selConstants(factory: String, p: ujson.Obj): Option[ujson.Obj] =
    if (factory != "rial.mathfunc") None
    else {
      val func = p("func").str
      val cfg  = mathConfig(func)
      val obj  = ujson.Obj()
      FuncKind.normalize(funcKinds(func)).foreach { k =>
        obj(k.toString) = ujson.Num(cfg.signal(k).litValue.toDouble)
      }
      obj("none") = ujson.Num(cfg.signalNone().litValue.toDouble)
      Some(obj)
    }

  private def emit(mod: => RawModule, outDir: String, name: String): Boolean = {
    new File(outDir).mkdirs()
    try {
      val verilog = ChiselStage.emitSystemVerilog(gen = mod, args = Array.empty, firtoolOpts = yosysFirtoolOpts)
      val w = new PrintWriter(new File(outDir, s"$name.sv"))
      w.write(verilog)
      w.close()
      true
    } catch {
      case e: Exception =>
        println(s"  FAILED $name: ${e.getMessage}")
        false
    }
  }

  // ------------------------------------------------------------------ run
  val manifestFile = new File(manifestPath)
  if (!manifestFile.exists()) {
    println(s"error: $manifestPath not found. Run: python3 scripts/build_manifest.py --plan-only")
    sys.exit(1)
  }

  val manifest = ujson.read(os.read(os.Path(manifestFile.getAbsolutePath)))
  val designs  = manifest("designs").arr

  println(s"Elaborating ${designs.length} designs from $manifestPath")

  var ok       = 0
  var failed   = 0
  val selConsts = scala.collection.mutable.LinkedHashMap.empty[String, ujson.Obj]

  designs.foreach { d =>
    val library  = d("library").str
    val stem     = d("stem").str
    val factory  = d("generator")("factory").str
    val params   = d("generator")("params").obj
    val outDir   = s"generated/$library"

    selConstants(factory, ujson.Obj.from(params)).foreach { v =>
      selConsts(s"$library/$stem") = v
    }

    if (emit(build(factory, ujson.Obj.from(params)), outDir, stem)) {
      ok += 1
      println(f"  [$ok%3d/${designs.length}%d] $library%-10s $stem")
    } else {
      failed += 1
    }
  }

  if (selConsts.nonEmpty) {
    val obj = ujson.Obj.from(selConsts.map { case (k, v) => k -> (v: ujson.Value) })
    os.write.over(os.pwd / "generated" / "generator_constants.json", ujson.write(obj, indent = 2))
    println(s"\nwrote generated/generator_constants.json (${selConsts.size} designs)")
  }

  println(s"\n$ok generated, $failed failed")
  if (failed > 0) sys.exit(1)
}
