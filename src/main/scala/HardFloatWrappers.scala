package hardfloat

import hardfloat._
import chisel3._
import chisel3.util._

class FPTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw: Int = expW + sigW
  val io = IO(new Bundle {
    val in  = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })

  val recfn = Wire(UInt((bw+1).W))
  recfn := recFNFromFN(expW, sigW, io.in)
  io.out := fNFromRecFN(expW, sigW, recfn)

  assert(io.out === io.in, "'out' should equal 'in'" )
}


class FPOPTest(val expW: Int = 8, val sigW: Int = 24, val mode: FPOPTestMode.Mode = FPOPTestMode.ADD) extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val in_a  = Input(UInt(bw.W))
    val in_b  = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })

  override def desiredName = s"FP${mode}_${expW}_$sigW"
  if (mode == FPOPTestMode.MUL) {
    val opRecFN = Module(new MulRecFN(expW, sigW))
    opRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
    opRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
    opRecFN.io.roundingMode := 0.U
    opRecFN.io.detectTininess := 0.U
    // ignore addRecFN.io.exceptionFlags for now
    io.out := fNFromRecFN(expW, sigW, opRecFN.io.out)

  } else if (mode == FPOPTestMode.ADD || mode == FPOPTestMode.SUB) {
    val opRecFN = Module(new AddRecFN(expW, sigW))
    opRecFN.io.subOp := false.B
    opRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
    if (mode == FPOPTestMode.SUB) {
      opRecFN.io.b := recFNFromFN(expW, sigW, Cat(~io.in_b(bw - 1), io.in_b(bw - 2, 0)))
    } else {
      opRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
    }
    opRecFN.io.roundingMode := 0.U
    opRecFN.io.detectTininess := 0.U
    // ignore addRecFN.io.exceptionFlags for now
    io.out := fNFromRecFN(expW, sigW, opRecFN.io.out)
  }
}


object FPOPTestMode {
  trait Mode
  case object ADD extends Mode
  case object SUB extends Mode
  case object MUL extends Mode
}


class FPDIVTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val divReady = Output(Bool())
    val valid = Input(Bool())
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val ready = Output(Bool())
    val out = Output(UInt(bw.W))
  })

  override def desiredName = s"FPDIV_${expW}_$sigW"

  val div = Module(new DivSqrtRecFN_small(expW, sigW, options = 0))

  io.divReady := div.io.inReady
  div.io.inValid := io.valid
  div.io.sqrtOp := false.B
  div.io.a := recFNFromFN(expW, sigW, io.in_a)
  div.io.b := recFNFromFN(expW, sigW, io.in_b)
  div.io.roundingMode := 0.U
  div.io.detectTininess := 0.U

  io.out := fNFromRecFN(expW, sigW, div.io.out)
  io.ready := div.io.outValid_div
}


class FPSqrtTest(val expW: Int = 8, val sigW: Int = 24) extends Module{
  val bw = expW + sigW
  val io = IO(new Bundle {
    val sqrtReady = Output(Bool())
    val valid = Input(Bool())
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val ready = Output(Bool())
    val out = Output(UInt(bw.W))
  })

  override def desiredName = s"FPSQRT_${expW}_$sigW"

  val sqrt = Module(new DivSqrtRecFN_small(expW, sigW, options = 0))

  io.sqrtReady := sqrt.io.inReady
  sqrt.io.inValid := io.valid
  sqrt.io.sqrtOp := true.B // Key difference from DIV
  sqrt.io.a := recFNFromFN(expW, sigW, io.in_a)
  sqrt.io.b := DontCare // Not used for SQRT
  sqrt.io.roundingMode := 0.U
  sqrt.io.detectTininess := 0.U

  io.out := fNFromRecFN(expW, sigW, sqrt.io.out)
  io.ready := sqrt.io.outValid_sqrt
}


class FPCompTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val in_a  = Input(UInt(bw.W))
    val in_b  = Input(UInt(bw.W))
    val out_eq = Output(UInt(1.W))
    val out_lt = Output(UInt(1.W))
    val out_gt = Output(UInt(1.W))
  })

  val compRecFN = Module(new CompareRecFN(expW, sigW))
  compRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
  compRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
  compRecFN.io.signaling := true.B // what does this do?
  // ignore addRecFN.io.exceptionFlags for now
  io.out_eq := compRecFN.io.eq
  io.out_lt := compRecFN.io.lt
  io.out_gt := compRecFN.io.gt
}
