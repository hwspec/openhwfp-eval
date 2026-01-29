package hardfloat

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import hardfloat.{FPDIVTest, FPOPTest, FPOPTestMode, FPTest, FPSqrtTest}
import java.lang.{Float => javaFloat}
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import scala.util.Random

class FPTestSpec32 extends AnyFlatSpec {
  behavior of "Hardfloat FP32 Tests"
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
    println("\n=================\nHardfloat FP32 FPTest BEGIN\n=================\n")
    simulate(new FPTest) { dut =>
      for (d <- testdata) {
        dut.io.in.poke(Float2Long(d))
        val result = dut.io.out.peek().litValue.toLong // litValue returns BigInt
        val resultFloat = Long2Float(result)
        if (debug) println(f"Hardfloat FP32: input=$d output=$resultFloat")
        dut.io.out.expect(Float2Long(d))
      }
    }
  }

  def testFPOPTest(m: FPOPTestMode.Mode): Unit = {
    val opName = m match {
      case FPOPTestMode.ADD => "ADD"
      case FPOPTestMode.SUB => "SUB"
      case FPOPTestMode.MUL => "MUL"
    }
    println(s"\n=================\nHardfloat FP32 FPOPTest $opName BEGIN\n=================\n")
    simulate(new FPOPTest(mode=m)) { dut =>
      for (d <- testdata.sliding(2)) {
        val a = d.head
        val b = d.tail.head
        val expected = m match {
          case FPOPTestMode.MUL => a * b
          case FPOPTestMode.SUB => a - b
          case _ => a + b
        }
        dut.io.in_a.poke(Float2Long(a))
        dut.io.in_b.poke(Float2Long(b))
        val result = dut.io.out.peek().litValue.toLong
        val resultFloat = Long2Float(result)
        if (debug) {
          val opstr = m match {
            case FPOPTestMode.MUL => "*"
            case FPOPTestMode.SUB => "-"
            case _ => "+"
          }
          println(f"Hardfloat FP32: $a $opstr $b should equal to $expected: out=$resultFloat")
        }
        dut.io.out.expect(Float2Long(expected))
      }
    }
  }

  "FPOPTest ADD" should "pass" in testFPOPTest(FPOPTestMode.ADD)

  "FPOPTest SUB" should "pass" in testFPOPTest(FPOPTestMode.SUB)

  "FPOPTest MUL" should "pass" in testFPOPTest(FPOPTestMode.MUL)

  def testFPDIVTest(): Unit = {
    println("\n=================\nHardfloat FP32 FPDIVTest BEGIN\n=================\n")
    simulate(new FPDIVTest()) { dut =>
      for (d <- testdata.sliding(2)) {
        val a = d.head
        val b = d.tail.head
        if (b != 0.0f) {
          var cycles: Int = 0
          val expected = a / b
          while (!dut.io.divReady.peek().litToBoolean) {
            dut.clock.step()
            cycles = cycles + 1
          }
          dut.io.in_a.poke(Float2Long(a))
          dut.io.in_b.poke(Float2Long(b))
          dut.io.valid.poke(true.B)
          dut.clock.step()
          cycles = cycles + 1
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!dut.io.ready.peek().litToBoolean) {
            dut.clock.step()
            cycles = cycles + 1
            if (!found && dut.io.divReady.peek().litToBoolean) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = dut.io.out.peek().litValue.toLong
          val resultFloat = Long2Float(result)
          if (debug) {
            println(f"Hardfloat FP32: $a / $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          }
          dut.io.out.expect(Float2Long(expected))
        }
      }
    }
  }

  "FPDIVTest" should "pass" in testFPDIVTest()



  "FPSqrtTest" should "pass" in testFPSqrtTest()
  def testFPSqrtTest(): Unit = {
    println("\n=================\nHardfloat FP32 FPSqrtTest BEGIN\n=================\n")
    simulate(new FPSqrtTest()) { dut =>
      for (d <- testdata) {
        if (d >= 0.0f){ //Only test non-nagative numbers
          var cycles: Int = 0
          val expected = Math.sqrt(d).toFloat
          while (!dut.io.sqrtReady.peek().litToBoolean){
            dut.clock.step()
            cycles = cycles + 1
          }
          dut.io.in_a.poke(Float2Long(d))
          dut.io.valid.poke(true.B)
          dut.clock.step()
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!dut.io.ready.peek().litToBoolean) {
            dut.clock.step()
            cycles = cycles + 1
            if (!found && dut.io.sqrtReady.peek().litToBoolean) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = dut.io.out.peek().litValue.toLong
          val resultFloat = Long2Float(result)
          if (debug) {
            println(f"Hardfloat FP32: sqrt($d) should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          }
          dut.io.out.expect(Float2Long(expected))
        }
      }
    }
  }

  // Note: Formal verification test removed - ChiselSIM doesn't support chiseltest.formal API
  // If formal verification is needed, consider using a different tool or framework
}



class FPTestSpec64 extends AnyFlatSpec {
  behavior of "Hardfloat FP64 Tests"
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
    val opName = m match {
      case FPOPTestMode.ADD => "ADD"
      case FPOPTestMode.SUB => "SUB"
      case FPOPTestMode.MUL => "MUL"
    }
    println(s"\n=================\nHardfloat FP64 FPOPTest $opName BEGIN\n=================\n")
    simulate(new FPOPTest(expW = 11, sigW = 53, mode = m)) { dut =>
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
          dut.io.in_a.poke(aBits.U(64.W))
          dut.io.in_b.poke(bBits.U(64.W))
          dut.clock.step(1) // Step clock to process inputs

          // Peek output
          val resultBits = dut.io.out.peek().litValue
          val resultDouble = uInt64ToDouble(resultBits)

          if (debug) {
            val opstr = m match {
              case FPOPTestMode.MUL => "*"
              case FPOPTestMode.SUB => "-"
              case _ => "+"
            }
            println(f"Hardfloat FP64: $a $opstr $b should equal $expected: out=$resultDouble")
          }

          // Expect output
          dut.io.out.expect(expectedBits.U(64.W))
        }
      }
    }
  }

  "FPOPTest ADD" should "pass" in testFPOPTest(FPOPTestMode.ADD)

  "FPOPTest SUB" should "pass" in testFPOPTest(FPOPTestMode.SUB)

  "FPOPTest MUL" should "pass" in testFPOPTest(FPOPTestMode.MUL)

  "FPTest div" should "pass" in testFPDIVTest()
  def testFPDIVTest(): Unit = {
      println("\n=================\nHardfloat FP64 FPDIVTest BEGIN\n=================\n")
      simulate(new FPDIVTest(11, 53)) { dut =>
        for (d <- testdata.sliding(2)) {
          val a = d.head
          val b = d.tail.head
          if (b != 0.0) {
            var cycles: Int = 0
            val expected = a / b
            while (!dut.io.divReady.peek().litToBoolean) {
              dut.clock.step()
              cycles = cycles + 1
            }
            dut.io.in_a.poke(doubleToUInt64(a))
            dut.io.in_b.poke(doubleToUInt64(b))
            dut.io.valid.poke(true.B)
            dut.clock.step()

            cycles = cycles + 1
            val waitready = cycles
            var throughput = 0
            var found = false
            while (!dut.io.ready.peek().litToBoolean) {
              dut.clock.step()
              cycles = cycles + 1
              if (!found && dut.io.divReady.peek().litToBoolean) {
                found = true
                throughput = cycles - waitready
              }
            }
            val result = dut.io.out.peek().litValue
            val resultDub = uInt64ToDouble(result)
            if (debug) {
              println(f"Hardfloat FP64: $a / $b should equal to $expected: out=$resultDub cycles=$cycles throughput=$throughput")
            }
            dut.io.out.expect(doubleToUInt64(expected))
          }
        }
      }
    }

  "FPTest sqrt" should "pass" in testFPSqrtTest()
  def testFPSqrtTest(): Unit = {
    println("\n=================\nHardfloat FP64 FPSqrtTest BEGIN\n=================\n")
    simulate(new FPSqrtTest(11, 53)) { dut =>
      for (d <- testdata) {
        if (d >= 0.0){ //Only test non-negative numbers
          var cycles: Int = 0
          val expected = Math.sqrt(d)
          while (!dut.io.sqrtReady.peek().litToBoolean){
            dut.clock.step()
            cycles = cycles + 1
          }
          dut.io.in_a.poke(doubleToUInt64(d))
          dut.io.valid.poke(true.B)
          dut.clock.step()
          val waitready = cycles
          var throughput = 0
          var found = false
          while (!dut.io.ready.peek().litToBoolean) {
            dut.clock.step()
            cycles = cycles + 1
            if (!found && dut.io.sqrtReady.peek().litToBoolean) {
              found = true
              throughput = cycles - waitready
            }
          }
          val result = dut.io.out.peek().litValue
          val resultDub = uInt64ToDouble(result)
          if (debug) {
            println(f"Hardfloat FP64: sqrt($d) should equal to $expected: out=$resultDub cycles=$cycles throughput=$throughput")
          }
          dut.io.out.expect(doubleToUInt64(expected))
        }
      }
    }
  }
}
