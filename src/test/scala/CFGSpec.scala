package cfg

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import FloatingPoint.fpu._

import scala.collection.mutable.ArrayBuffer
import java.lang.{Float => javaFloat}
import java.lang.{Math => javaMath}
import scala.util.Random


//Helper functions for CFGBasic
object halfBits {
  def floatToHalfBits(value: Float): Int = {
    val fbits = java.lang.Float.floatToIntBits(value)
    val sign = (fbits >> 31) & 0x1
    val exp = (fbits >> 23) & 0xFF
    val frac = fbits & 0x7FFFFF

    var halfSign = sign << 15
    var halfExp = 0
    var halfFrac = 0

    if (exp == 255) {
      // Inf or NaN
      halfExp = 0x1F
      halfFrac = if (frac != 0) 0x200 else 0 // preserve NaN
    } else if (exp > 142) {
      // Overflow -> set to Inf
      halfExp = 0x1F
    } else if (exp < 113) {
      // Subnormal or zero
      val shift = 113 - exp
      if (shift < 24) {
        halfFrac = (frac | 0x800000) >> shift
        if ((halfFrac & 0x00001000) != 0) {
          halfFrac += 0x00002000 // round to nearest
        }
        halfFrac >>= 13
      }
      halfExp = 0
    } else {
      // Normalized value
      halfExp = exp - 112
      halfFrac = frac >> 13
      if ((frac & 0x1000) != 0) {
        // Round to nearest even
        val incr = 1
        halfFrac += incr
        if (halfFrac == 0x400) {
          halfFrac = 0
          halfExp += 1
        }
      }
    }

    val result = halfSign | (halfExp << 10) | (halfFrac & 0x3FF)
    result & 0xFFFF
  }

  def halfBitsToFloat(bits: Int): Float = {
    val sign = (bits >> 15) & 0x1
    val exp = (bits >> 10) & 0x1F
    val frac = bits & 0x3FF

    val fsign: Int = sign << 31
    var fexp: Int = 0
    var ffrac: Int = 0

    if (exp == 0) {
      if (frac == 0) {
        fexp = 0
        ffrac = 0
      } else {
        // Subnormal number
        var e = -1
        var fr = frac
        while ((fr & 0x400) == 0) {
          e -= 1
          fr <<= 1
        }
        fr &= 0x3FF
        fexp = 127 - 15 + 1 + e
        ffrac = fr << 13
      }
    } else if (exp == 0x1F) {
      fexp = 0xFF
      ffrac = if (frac != 0) 0x7FFFFF else 0
    } else {
      fexp = exp + (127 - 15)
      ffrac = frac << 13
    }

    val fbits = fsign | (fexp << 23) | ffrac
    java.lang.Float.intBitsToFloat(fbits)
  }

}


//used to test the different pipeline depths at different bit widths of addition, subtraction, and multiplication operations
class CFGBasic extends AnyFlatSpec {

  val nrndtests = 20
  val rnd = new Random()
  val fixedtestdata: List[Float] = List(0f, 1f, -1f, 2f)
  val rndtestdata: List[Float] = List.fill(nrndtests) {
    rnd.between(-1.0e4f, 1.0e4f)
  }
  val testdata: List[Float] = fixedtestdata.concat(rndtestdata)

  val pipe: List[Int] = List(1, 3, 7, 10, 11, 13)
  val width: List[Int] = List(16, 32, 64)

  def iterTest(): Unit = {
    for (pd <- pipe) {
      for (bw <- width) {
        println(f"\nBW = $bw PD = $pd")

        //Addition
        simulate(new FP_add(bw, pd)) { dut =>
          var expectedVals = new Array[Float](testdata.length - 1)
          var outputs = new Array[UInt](testdata.length - 1)

          var aVals = ArrayBuffer[Float]()
          var bVals = ArrayBuffer[Float]()

          dut.io.in_en.poke(true.B)
          dut.io.in_valid.poke(true.B)
          var cycles: Int = 0
          for (d <- testdata.sliding(2)) {
            val a = d.head
            val b = d.tail.head

            aVals += a
            bVals += b
            expectedVals(cycles) = (a + b)

            if (bw == 16) {
              dut.io.in_a.poke(halfBits.floatToHalfBits(a))
              dut.io.in_b.poke(halfBits.floatToHalfBits(b))
            }
            else if (bw == 32) {
              dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
              dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
            }
            else if (bw == 64) {
              val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
              dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

              val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
              dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
            }

            if (cycles >= pd) {
              outputs(cycles - pd) = dut.io.out_s.peek()
            }

            dut.clock.step()
            cycles += 1

          }

          dut.io.in_valid.poke(false.B)
          val throughput = cycles / (testdata.length - 1)

          for (i <- 0 until pd) {
            outputs(cycles - pd) = dut.io.out_s.peek()
            dut.clock.step()
            cycles += 1
          }

          for (i <- 0 until expectedVals.length) {
            val a = aVals(i)
            val b = bVals(i)
            val expected = expectedVals(i)
            val res: UInt = outputs(i)

            var resultFloat = 0.0
            if (bw == 16) {
              resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
            }
            if (bw == 32) {
              resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
            }
            if (bw == 64) {
              val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
              resultFloat = java.lang.Double.longBitsToDouble(longBits)
            }

            println(f"CFG $a + $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
            dut.clock.step()
            cycles += 1
          }
        }

        //Subtraction
        simulate(new FP_add(bw, pd)) { dut =>
          var expectedVals = new Array[Float](testdata.length - 1)
          var outputs = new Array[UInt](testdata.length - 1)

          var aVals = ArrayBuffer[Float]()
          var bVals = ArrayBuffer[Float]()

          dut.io.in_en.poke(true.B)
          dut.io.in_valid.poke(true.B)
          var cycles: Int = 0
          for (d <- testdata.sliding(2)) {
            val a = d.head
            val b = d.tail.head

            aVals += a
            bVals += b
            expectedVals(cycles) = (a - b)

            if (bw == 16) {
              dut.io.in_a.poke(halfBits.floatToHalfBits(a))
              dut.io.in_b.poke(halfBits.floatToHalfBits(b))
            }
            else if (bw == 32) {
              dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
              dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
            }
            else if (bw == 64) {
              val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
              dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

              val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
              dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
            }

            if (cycles >= pd) {
              outputs(cycles - pd) = dut.io.out_s.peek()
            }

            dut.clock.step()
            cycles += 1

          }

          dut.io.in_valid.poke(false.B)
          val throughput = cycles / (testdata.length - 1)

          for (i <- 0 until pd) {
            outputs(cycles - pd) = dut.io.out_s.peek()
            dut.clock.step()
            cycles += 1
          }

          for (i <- 0 until expectedVals.length) {
            val a = aVals(i)
            val b = bVals(i)
            val expected = expectedVals(i)
            val res: UInt = outputs(i)

            var resultFloat = 0.0
            if (bw == 16) {
              resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
            }
            if (bw == 32) {
              resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
            }
            if (bw == 64) {
              val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
              resultFloat = java.lang.Double.longBitsToDouble(longBits)
            }

            println(f"CFG $a - $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
            dut.clock.step()
            cycles += 1
          }
        }

        //Multiplication
        if (pd != 11) {
          simulate(new FP_mult(bw, pd)) { dut =>
            var expectedVals = new Array[Float](testdata.length - 1)
            var outputs = new Array[UInt](testdata.length - 1)

            var aVals = ArrayBuffer[Float]()
            var bVals = ArrayBuffer[Float]()

            dut.io.in_en.poke(true.B)
            dut.io.in_valid.poke(true.B)
            var cycles: Int = 0
            for (d <- testdata.sliding(2)) {
              val a = d.head
              val b = d.tail.head

              aVals += a
              bVals += b
              expectedVals(cycles) = (a * b)

              if (bw == 16) {
                dut.io.in_a.poke(halfBits.floatToHalfBits(a))
                dut.io.in_b.poke(halfBits.floatToHalfBits(b))
              }
              else if (bw == 32) {
                dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
                dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
              }
              else if (bw == 64) {
                val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
                dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

                val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
                dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
              }

              if (cycles >= pd) {
                outputs(cycles - pd) = dut.io.out_s.peek()
              }

              dut.clock.step()
              cycles += 1

            }

            dut.io.in_valid.poke(false.B)
            val throughput = cycles / (testdata.length - 1)

            for (i <- 0 until pd) {
              outputs(cycles - pd) = dut.io.out_s.peek()
              dut.clock.step()
              cycles += 1
            }

            for (i <- 0 until expectedVals.length) {
              val a = aVals(i)
              val b = bVals(i)
              val expected = expectedVals(i)
              val res: UInt = outputs(i)

              var resultFloat = 0.0
              if (bw == 16) {
                resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
              }
              if (bw == 32) {
                resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
              }
              if (bw == 64) {
                val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
                resultFloat = java.lang.Double.longBitsToDouble(longBits)
              }

              println(f"CFG $a * $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
              dut.clock.step()
              cycles += 1
            }
          }

        }
      }

    }
  }

  "FP" should "pass" in iterTest()
}

  //to test only this class, run 'testOnly cfg.CFGTest'
  class CFGTest extends AnyFlatSpec {
    val nrndtests = 20
    val rnd = new Random()
    val fixedtestdata: List[Float] = List(0f, 1f, -1f, 2f)
    val rndtestdata: List[Float] = List.fill(nrndtests) {
      rnd.between(-1.0e4f, 1.0e4f)
    }
    val testdata: List[Float] = fixedtestdata.concat(rndtestdata)

    var expectedVals = new Array[Float](nrndtests)
    var outputs = new Array[UInt](nrndtests)

    var aVals = ArrayBuffer[Float]()
    var bVals = ArrayBuffer[Float]()

    val bw = 32
    val pd = 1

    def nmantissabits(bw: Int): Int = {
      val (exp, mant) = bw match {
        case 16 => (5, 10)
        case 32 => (8, 23)
        case 64 => (11, 52)
        case 128 => (15, 112)
      }
      mant
    }

    def testFP_add(): Unit = {
      simulate(new FP_add(bw, pd)) { dut =>
        var expectedVals = new Array[Float](testdata.length - 1)
        var outputs = new Array[UInt](testdata.length - 1)

        var aVals = ArrayBuffer[Float]()
        var bVals = ArrayBuffer[Float]()

        dut.io.in_en.poke(true.B)
        dut.io.in_valid.poke(true.B)
        var cycles: Int = 0
        for (d <- testdata.sliding(2)) {
          val a = d.head
          val b = d.tail.head

          aVals += a
          bVals += b
          expectedVals(cycles) = (a + b)

          if (bw == 16) {
            dut.io.in_a.poke(halfBits.floatToHalfBits(a))
            dut.io.in_b.poke(halfBits.floatToHalfBits(b))
          }
          else if (bw == 32) {
            dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
            dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
          }
          else if (bw == 64) {
            val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
            dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

            val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
            dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
          }

          if (cycles >= pd) {
            outputs(cycles - pd) = dut.io.out_s.peek()
          }

          dut.clock.step()
          cycles += 1

        }

        dut.io.in_valid.poke(false.B)
        val throughput = cycles / (testdata.length - 1)

        for (i <- 0 until pd) {
          outputs(cycles - pd) = dut.io.out_s.peek()
          dut.clock.step()
          cycles += 1
        }

        for (i <- 0 until expectedVals.length) {
          val a = aVals(i)
          val b = bVals(i)
          val expected = expectedVals(i)
          val res: UInt = outputs(i)

          var resultFloat = 0.0
          if (bw == 16) {
            resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
          }
          if (bw == 32) {
            resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
          }
          if (bw == 64) {
            val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
            resultFloat = java.lang.Double.longBitsToDouble(longBits)
          }

          println(f"CFG $a + $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          dut.clock.step()
          cycles += 1
        }
      }
    }

    "FP_Add" should "pass" in testFP_add()

    //Subtraction
    def testFP_sub(): Unit = {
      simulate(new FP_add(bw, pd)) { dut =>
        var expectedVals = new Array[Float](testdata.length - 1)
        var outputs = new Array[UInt](testdata.length - 1)

        var aVals = ArrayBuffer[Float]()
        var bVals = ArrayBuffer[Float]()

        dut.io.in_en.poke(true.B)
        dut.io.in_valid.poke(true.B)
        var cycles: Int = 0
        for (d <- testdata.sliding(2)) {
          val a = d.head
          val b = d.tail.head

          aVals += a
          bVals += b
          expectedVals(cycles) = (a - b)

          if (bw == 16) {
            dut.io.in_a.poke(halfBits.floatToHalfBits(a))
            dut.io.in_b.poke(halfBits.floatToHalfBits(b))
          }
          else if (bw == 32) {
            dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
            dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
          }
          else if (bw == 64) {
            val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
            dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

            val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
            dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
          }

          if (cycles >= pd) {
            outputs(cycles - pd) = dut.io.out_s.peek()
          }

          dut.clock.step()
          cycles += 1

        }

        dut.io.in_valid.poke(false.B)
        val throughput = cycles / (testdata.length - 1)

        for (i <- 0 until pd) {
          outputs(cycles - pd) = dut.io.out_s.peek()
          dut.clock.step()
          cycles += 1
        }

        for (i <- 0 until expectedVals.length) {
          val a = aVals(i)
          val b = bVals(i)
          val expected = expectedVals(i)
          val res: UInt = outputs(i)

          var resultFloat = 0.0
          if (bw == 16) {
            resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
          }
          if (bw == 32) {
            resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
          }
          if (bw == 64) {
            val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
            resultFloat = java.lang.Double.longBitsToDouble(longBits)
          }

          println(f"CFG $a - $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
          dut.clock.step()
          cycles += 1
        }
      }
    }
      "FP_Sub" should "pass" in testFP_sub()

      //Multiplication
      def testFP_mult(): Unit = {
        simulate(new FP_mult(bw, pd)) { dut =>
          var expectedVals = new Array[Float](testdata.length - 1)
          var outputs = new Array[UInt](testdata.length - 1)

          var aVals = ArrayBuffer[Float]()
          var bVals = ArrayBuffer[Float]()

          dut.io.in_en.poke(true.B)
          dut.io.in_valid.poke(true.B)
          var cycles: Int = 0
          for (d <- testdata.sliding(2)) {
            val a = d.head
            val b = d.tail.head

            aVals += a
            bVals += b
            expectedVals(cycles) = (a * b)

            if (bw == 16) {
              dut.io.in_a.poke(halfBits.floatToHalfBits(a))
              dut.io.in_b.poke(halfBits.floatToHalfBits(b))
            }
            else if (bw == 32) {
              dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
              dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
            }
            else if (bw == 64) {
              val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
              dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

              val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
              dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
            }

            if (cycles >= pd) {
              outputs(cycles - pd) = dut.io.out_s.peek()
            }

            dut.clock.step()
            cycles += 1

          }

          dut.io.in_valid.poke(false.B)
          val throughput = cycles / (testdata.length - 1)

          for (i <- 0 until pd) {
            outputs(cycles - pd) = dut.io.out_s.peek()
            dut.clock.step()
            cycles += 1
          }

          for (i <- 0 until expectedVals.length) {
            val a = aVals(i)
            val b = bVals(i)
            val expected = expectedVals(i)
            val res: UInt = outputs(i)

            var resultFloat = 0.0
            if (bw == 16) {
              resultFloat = halfBits.halfBitsToFloat(res.litValue.toInt)
            }
            if (bw == 32) {
              resultFloat = javaFloat.intBitsToFloat(res.litValue.toLong.toInt)
            }
            if (bw == 64) {
              val longBits = (res.litValue & ((BigInt(1) << 64) - 1)).toLong
              resultFloat = java.lang.Double.longBitsToDouble(longBits)
            }

            println(f"CFG $a * $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
            dut.clock.step()
            cycles += 1
          }
        }
      }

      "FP_Mult" should "pass" in testFP_mult()

      //Division
      def testFP_div(): Unit = {
        // FP_div(bw, L, latency)
        simulate(new FP_div(bw, 15, 15)) { dut =>
          for (d <- testdata.sliding(2)) {
            val a = d.head
            val b = d.tail.head
            if (b != 0.0f) {
              var cycles: Int = 0
              val expected = a / b

              while (dut.io.out_valid.peek().litToBoolean) {
                dut.clock.step(1)
                cycles += 1
              }
              val waitready = cycles
              var throughput = 0

              //Initialization
              if (bw == 16) {
                dut.io.in_a.poke(halfBits.floatToHalfBits(a))
                dut.io.in_b.poke(halfBits.floatToHalfBits(b))
              }
              else if (bw == 32) {
                dut.io.in_a.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
                dut.io.in_b.poke(javaFloat.floatToRawIntBits(b) & 0xffffffffL)
              }
              else if (bw == 64) {
                val rawLong_a = java.lang.Double.doubleToRawLongBits(a)
                dut.io.in_a.poke(BigInt(rawLong_a) & ((BigInt(1) << 64) - 1))

                val rawLong_b = java.lang.Double.doubleToRawLongBits(b)
                dut.io.in_b.poke(BigInt(rawLong_b) & ((BigInt(1) << 64) - 1))
              }

              dut.io.in_valid.poke(true.B)
              dut.io.in_en.poke(true.B)
              dut.clock.step(1)
              cycles += 1
              dut.io.in_valid.poke(false.B)

              while (!dut.io.out_valid.peek().litToBoolean) {
                dut.clock.step(1)
                cycles += 1
              }


              throughput = cycles - waitready
              var resultFloat = 0.0
              if (bw == 16) {
                resultFloat = halfBits.halfBitsToFloat(dut.io.out_s.peek().litValue.toInt)
              }
              if (bw == 32) {
                resultFloat = javaFloat.intBitsToFloat(dut.io.out_s.peek().litValue.toLong.toInt)
              }
              if (bw == 64) {
                val longBits = (dut.io.out_s.peek().litValue & ((BigInt(1) << 64) - 1)).toLong
                resultFloat = java.lang.Double.longBitsToDouble(longBits)
              }
              println(f"CFG $a / $b should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
            }


          }
        }
      }

      "FP_Div" should "pass" in testFP_div()


      //Sqrt
      def testFP_sqrt(): Unit = {
        // FP_sqrt(bw, L, latency)
        simulate(new FP_sqrt(bw, nmantissabits(bw), nmantissabits(bw))) { dut =>
          for (s <- testdata) {
            if (s >= 0.0f) {
              var cycles: Int = 0
              val expected = Math.sqrt(s).toFloat

              val wait = cycles
              var throughput = 0

              //Initialization
              dut.io.in_en.poke(true.B)
              dut.io.in_valid.poke(true.B)

              if (bw == 16) {
                dut.io.in_a.poke(halfBits.floatToHalfBits(s))
              }
              else if (bw == 32) {
                dut.io.in_a.poke(javaFloat.floatToRawIntBits(s) & 0xffffffffL)
              }
              else if (bw == 64) {
                val rawLong = java.lang.Double.doubleToRawLongBits(s)
                dut.io.in_a.poke(BigInt(rawLong) & ((BigInt(1) << 64) - 1))
              }

              dut.clock.step(1)
              cycles += 1
              dut.io.in_valid.poke(false.B)

              while (!dut.io.out_valid.peek().litToBoolean) {
                dut.clock.step(1)
                cycles += 1
              }

              throughput = cycles - wait
              var resultFloat = 0.0
              if (bw == 16) {
                resultFloat = halfBits.halfBitsToFloat(dut.io.out_s.peek().litValue.toInt)
              }
              if (bw == 32) {
                resultFloat = javaFloat.intBitsToFloat(dut.io.out_s.peek().litValue.toLong.toInt)
              }
              if (bw == 64) {
                val longBits = (dut.io.out_s.peek().litValue & ((BigInt(1) << 64) - 1)).toLong
                resultFloat = java.lang.Double.longBitsToDouble(longBits)
              }
              println(f"CFG sqrt($s) should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")
            }
          }
        }

      }

      "FP_Sqrt" should "pass" in testFP_sqrt()

      //Cos
      def testFP_cos(): Unit = {
        simulate(new FP_cos(bw, nmantissabits(bw))) { dut =>
          for (a <- testdata) {
            var cycles: Int = 0
            val expected = javaMath.cos(a)

            while (dut.io.out_valid.peek().litToBoolean) {
              dut.clock.step(1)
              cycles += 1
            }
            val waitready = cycles
            var throughput = 0

            //Initialization
            dut.io.in_valid.poke(true.B)
            dut.io.in_en.poke(true.B)
            if (bw == 16) {
              dut.io.in_angle.poke(halfBits.floatToHalfBits(a))
            }
            else if (bw == 32) {
              dut.io.in_angle.poke(javaFloat.floatToRawIntBits(a) & 0xffffffffL)
            }
            else if (bw == 64) {
              val rawLong = java.lang.Double.doubleToRawLongBits(a)
              dut.io.in_angle.poke(BigInt(rawLong) & ((BigInt(1) << 64) - 1))
            }

            dut.clock.step(1)
            cycles += 1
            dut.io.in_valid.poke(false.B)

            while (!dut.io.out_valid.peek().litToBoolean) {
              dut.clock.step(1)
              cycles += 1
            }

            throughput = cycles - waitready
            var resultFloat = 0.0
            if (bw == 16) {
              resultFloat = halfBits.halfBitsToFloat(dut.io.out_cos.peek().litValue.toInt)
            }
            if (bw == 32) {
              resultFloat = javaFloat.intBitsToFloat(dut.io.out_cos.peek().litValue.toLong.toInt)
            }
            if (bw == 64) {
              val longBits = (dut.io.out_cos.peek().litValue & ((BigInt(1) << 64) - 1)).toLong
              resultFloat = java.lang.Double.longBitsToDouble(longBits)
            }
            println(f"CFG cos($a) should equal to $expected: out=$resultFloat cycles=$cycles throughput=$throughput")

          }
        }
      }

      "FP_Cos" should "pass" in testFP_cos()

      // NOTE: FP_sin is not available in the current OpenFloat fpu.
      // The original CFG sin test is disabled until a matching module exists.



  }

