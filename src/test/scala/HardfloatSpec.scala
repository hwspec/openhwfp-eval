package xs

import chisel3._
import chiseltest._
import chiseltest.formal.{BoundedCheck, Formal}
import org.scalatest.flatspec.AnyFlatSpec
//import xs.{FPDIVTest, FPOPTest, FPTest}

import java.lang.{Float => javaFloat}
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import scala.util.Random

class FPTestSpec32 extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "FPtest"
  val debug = true
  val nrndtests = 20
  val rnd = new Random()
  val fixedtestdata: List[Float] = List(0f, 1f, -1f, 2f)
  val rndtestdata: List[Float] = List.fill(nrndtests) { rnd.between(-1.0e4f, 1.0e4f) }
  val testdata :List[Float] = fixedtestdata.concat(rndtestdata)

  // due to the lack of 32-bit unsigned int in Scala, we need to use Long
  def Float2Long(v: Float): Long = javaFloat.floatToRawIntBits(v) & 0xffffffffL // note 'L' is needed
  def Long2Float(v: Long): Float = javaFloat.intBitsToFloat(v.toInt)

  // this just does convert IEEE float into RecFN and back to IEEE float
  "FPTest" should "pass" in {
    test(new FPTest) { c =>
      for (d <- testdata) {
        c.io.in.poke(Float2Long(d))
        val result = c.io.out.peek().litValue.toLong // litValue returns BigInt
        val resultFloat = Long2Float(result)
        if (debug) println(f"input=$d  output=$resultFloat")
        c.io.out.expect(Float2Long(d))
      }
    }
  }

  def testFPOPTest(m: FPOPTestMode.Mode): Unit = {
    test(new FPOPTest(mode=m)) { c =>
      for (d <- testdata.sliding(2)) {
        val a = d.head
        val b = d.tail.head
        val expected = m match {
          case FPOPTestMode.MUL => a * b
          case FPOPTestMode.SUB => a - b
          case _ => a + b
        }
        c.io.in_a.poke(Float2Long(a))
        c.io.in_b.poke(Float2Long(b))
        val result = c.io.out.peek().litValue.toLong
        val resultFloat = Long2Float(result)
        if (debug) {
          val opstr = m match {
            case FPOPTestMode.MUL => "*"
            case FPOPTestMode.SUB => "-"
            case _ => "+"
          }
          println(f"$a $opstr $b should equal to $expected: $resultFloat")
        }
        c.io.out.expect(Float2Long(expected))
      }
    }
  }

  "FPOPTest ADD" should "pass" in testFPOPTest(FPOPTestMode.ADD)

  "FPOPTest SUB" should "pass" in testFPOPTest(FPOPTestMode.SUB)

  "FPOPTest MUL" should "pass" in testFPOPTest(FPOPTestMode.MUL)

  def testFPDIVTest(): Unit = {
    test(new FPDIVTest()) { c =>
      for (d <- testdata.sliding(2)) {
        val a = d.head
        val b = d.tail.head
        if (b != 0.0f) {
          var cycles: Int = 0
          val expected = a / b
          while (!c.io.divReady.peekBoolean()) {
            c.clock.step()
            cycles = cycles + 1
          }
          c.io.in_a.poke(Float2Long(a))
          c.io.in_b.poke(Float2Long(b))
          c.io.valid.poke(true.B)
          c.clock.step()
          cycles = cycles + 1
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!c.io.ready.peekBoolean()) {
            c.clock.step()
            cycles = cycles + 1
            if (!found && c.io.divReady.peekBoolean()) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = c.io.out.peek().litValue.toLong
          val resultFloat = Long2Float(result)
          if (debug) {
            println(f"$a is divided by $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          }
          c.io.out.expect(Float2Long(expected))
        }
      }
    }
  }

  "FPDIVTest" should "pass" in testFPDIVTest()



  "FPSqrtTest" should "pass" in testFPSqrtTest()
  def testFPSqrtTest(): Unit = {
    test(new FPSqrtTest()) { c =>
      for (d <- testdata) {
        if (d >= 0.0f){ //Only test non-nagative numbers
          var cycles: Int = 0
          val expected = Math.sqrt(d).toFloat
          while (!c.io.sqrtReady.peekBoolean()){
            c.clock.step()
            cycles = cycles + 1
          }
          c.io.in_a.poke(Float2Long(d))
          c.io.valid.poke(true.B)
          c.clock.step()
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!c.io.ready.peekBoolean()) {
            c.clock.step()
            cycles = cycles + 1
            if (!found && c.io.sqrtReady.peekBoolean()) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = c.io.out.peek().litValue.toLong
          val resultFloat = Long2Float(result)
          if (debug) {
            println(f"sqrt($d) should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          }
          c.io.out.expect(Float2Long(expected))
        }
      }
    }
  }


  //
  // format test
  //

  "Check the identity of FPTest" should "pass" in {
    verify(new FPTest, Seq(BoundedCheck(1)))
  }
}



class FPTestSpec64 extends AnyFlatSpec with ChiselScalatestTester with Formal {
  behavior of "FPtest"
  val debug = true
  val nrndtests = 20
  val rnd = new Random()
  val fixedtestdata: List[Double] = List(0.0, 1.0, -1.0, 2.0)
  val rndtestdata: List[Double] = List.fill(nrndtests) { rnd.between(-1.0e4, 1.0e4) }
  val testdata: List[Double] = fixedtestdata.concat(rndtestdata)

  // Helper function to convert Double to IEEE 754 64-bit unsigned representation as BigInt
  def doubleToUInt64(d: Double): BigInt = {
    val bits = doubleToLongBits(d)
    BigInt(bits) & BigInt("FFFFFFFFFFFFFFFF", 16) // Mask to ensure unsigned 64-bit
  }

  // Helper function to convert IEEE 754 64-bit unsigned BigInt to Double
  def uInt64ToDouble(bits: BigInt): Double = {
    longBitsToDouble(bits.toLong)
  }

  def testFPOPTest(m: FPOPTestMode.Mode): Unit = {
    test(new FPOPTest(expW = 11, sigW = 53, mode = m)) { c =>
      for (d <- testdata.sliding(2, 2)) {
        if (d.length == 2) { // Ensure we have a pair
          val a = d.head
          val b = d.tail.head
          val expected = m match {
            case FPOPTestMode.MUL => a * b
            case FPOPTestMode.SUB => a - b
            case _ => a + b
          }

          // Convert inputs and expected output to IEEE 754 64-bit representation
          val aBits = doubleToUInt64(a)
          val bBits = doubleToUInt64(b)
          val expectedBits = doubleToUInt64(expected)

          // Poke inputs
          c.io.in_a.poke(aBits.U(64.W))
          c.io.in_b.poke(bBits.U(64.W))
          c.clock.step(1) // Step clock to process inputs

          // Peek output
          val resultBits = c.io.out.peek().litValue
          val resultDouble = uInt64ToDouble(resultBits)

          if (debug) {
            val opstr = m match {
              case FPOPTestMode.MUL => "*"
              case FPOPTestMode.SUB => "-"
              case _ => "+"
            }
            println(f"$a $opstr $b should equal $expected (bits: 0x$expectedBits%X), got: $resultDouble (bits: 0x$resultBits%X)")
          }

          // Expect output
          c.io.out.expect(expectedBits.U(64.W))
        }
      }
    }
  }

  "FPOPTest ADD" should "pass" in testFPOPTest(FPOPTestMode.ADD)

  "FPOPTest SUB" should "pass" in testFPOPTest(FPOPTestMode.SUB)

  "FPOPTest MUL" should "pass" in testFPOPTest(FPOPTestMode.MUL)

  "FPTest div" should "pass" in testFPDIVTest()
  def testFPDIVTest(): Unit = {
      test(new FPDIVTest(11, 53)) { c =>
        for (d <- testdata.sliding(2)) {
          val a = d.head
          val b = d.tail.head
          if (b != 0.0) {
            var cycles: Int = 0
            val expected = a / b
            while (!c.io.divReady.peekBoolean()) {
              c.clock.step()
              cycles = cycles + 1
            }
            c.io.in_a.poke(doubleToUInt64(a))
            c.io.in_b.poke(doubleToUInt64(b))
            c.io.valid.poke(true.B)
            c.clock.step()

            cycles = cycles + 1
            val waitready = cycles
            var throughput = 0
            var found = false
            while (!c.io.ready.peekBoolean()) {
              c.clock.step()
              cycles = cycles + 1
              if (!found && c.io.divReady.peekBoolean()) {
                found = true
                throughput = cycles - waitready
              }
            }
            val result = c.io.out.peek().litValue
            val resultDub = uInt64ToDouble(result)
            if (debug) {
              println(f"$a is divided by $b should equal to $expected: out=$resultDub cycles=$cycles throughput=$throughput")
            }
            c.io.out.expect(doubleToUInt64(expected))
          }
        }
      }
    }

  "FPTest sqrt" should "pass" in testFPSqrtTest()
  def testFPSqrtTest(): Unit = {
    test(new FPSqrtTest(11, 53)) { c =>
      for (d <- testdata) {
        if (d >= 0.0){ //Only test non-negative numbers
          var cycles: Int = 0
          val expected = Math.sqrt(d)
          while (!c.io.sqrtReady.peekBoolean()){
            c.clock.step()
            cycles = cycles + 1
          }
          c.io.in_a.poke(doubleToUInt64(d))
          c.io.valid.poke(true.B)
          c.clock.step()
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!c.io.ready.peekBoolean()) {
            c.clock.step()
            cycles = cycles + 1
            if (!found && c.io.sqrtReady.peekBoolean()) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = c.io.out.peek().litValue
          val resultDub = uInt64ToDouble(result)
          if (debug) {
            println(f"sqrt($d) should equal to $expected: out=$resultDub cycles=$cycles throughput=$throughput")
          }
          c.io.out.expect(doubleToUInt64(expected))
        }
      }
    }
  }
}
