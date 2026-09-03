package hardfloat

import hardfloat._
import chisel3._
import chisel3.util._

class FPControl extends Bundle {
  val roundingMode   = Input(UInt(3.W))
  val detectTininess = Input(UInt(1.W))
}

class FPTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw: Int = expW + sigW
  val io = IO(new Bundle {
    val in  = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })

  override def desiredName = s"FPTest_${expW}_$sigW"

  val recfn = Wire(UInt((bw + 1).W))
  recfn := recFNFromFN(expW, sigW, io.in)
  io.out := fNFromRecFN(expW, sigW, recfn)
}


class FPOPTest(val expW: Int = 8, val sigW: Int = 24, val mode: FPOPTestMode.Mode = FPOPTestMode.ADD)
    extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val ctrl = new FPControl
    val out  = Output(UInt(bw.W))
    val exceptionFlags = Output(UInt(5.W))
  })

  override def desiredName = s"FP${mode}_${expW}_$sigW"

  if (mode == FPOPTestMode.MUL) {
    val opRecFN = Module(new MulRecFN(expW, sigW))
    opRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
    opRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
    opRecFN.io.roundingMode   := io.ctrl.roundingMode
    opRecFN.io.detectTininess := io.ctrl.detectTininess
    io.out := fNFromRecFN(expW, sigW, opRecFN.io.out)
    io.exceptionFlags := opRecFN.io.exceptionFlags
  } else {
    // subOp is the core's own subtract. Flipping b's sign bit instead would rewrite NaN operands,
    // which testfloat notices.
    val opRecFN = Module(new AddRecFN(expW, sigW))
    opRecFN.io.subOp := (mode == FPOPTestMode.SUB).B
    opRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
    opRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
    opRecFN.io.roundingMode   := io.ctrl.roundingMode
    opRecFN.io.detectTininess := io.ctrl.detectTininess
    io.out := fNFromRecFN(expW, sigW, opRecFN.io.out)
    io.exceptionFlags := opRecFN.io.exceptionFlags
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
    val valid    = Input(Bool())
    val in_a     = Input(UInt(bw.W))
    val in_b     = Input(UInt(bw.W))
    val ctrl     = new FPControl
    val ready    = Output(Bool())
    val out      = Output(UInt(bw.W))
    val exceptionFlags = Output(UInt(5.W))
  })

  override def desiredName = s"FPDIV_${expW}_$sigW"

  val div = Module(new DivSqrtRecFN_small(expW, sigW, options = 0))

  io.divReady := div.io.inReady
  div.io.inValid := io.valid
  div.io.sqrtOp  := false.B
  div.io.a := recFNFromFN(expW, sigW, io.in_a)
  div.io.b := recFNFromFN(expW, sigW, io.in_b)
  div.io.roundingMode   := io.ctrl.roundingMode
  div.io.detectTininess := io.ctrl.detectTininess

  io.out   := fNFromRecFN(expW, sigW, div.io.out)
  io.ready := div.io.outValid_div
  io.exceptionFlags := div.io.exceptionFlags
}


class FPSqrtTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val sqrtReady = Output(Bool())
    val valid     = Input(Bool())
    val in_a      = Input(UInt(bw.W))
    val in_b      = Input(UInt(bw.W))
    val ctrl      = new FPControl
    val ready     = Output(Bool())
    val out       = Output(UInt(bw.W))
    val exceptionFlags = Output(UInt(5.W))
  })

  override def desiredName = s"FPSQRT_${expW}_$sigW"

  val sqrt = Module(new DivSqrtRecFN_small(expW, sigW, options = 0))

  io.sqrtReady := sqrt.io.inReady
  sqrt.io.inValid := io.valid
  sqrt.io.sqrtOp  := true.B
  sqrt.io.a := recFNFromFN(expW, sigW, io.in_a)
  // The core ignores b for sqrt. Kept on the wrapper so the port list matches DIV.
  sqrt.io.b := recFNFromFN(expW, sigW, io.in_b)
  sqrt.io.roundingMode   := io.ctrl.roundingMode
  sqrt.io.detectTininess := io.ctrl.detectTininess

  io.out   := fNFromRecFN(expW, sigW, sqrt.io.out)
  io.ready := sqrt.io.outValid_sqrt
  io.exceptionFlags := sqrt.io.exceptionFlags
}


class FPCompTest(val expW: Int = 8, val sigW: Int = 24) extends Module {
  val bw = expW + sigW
  val io = IO(new Bundle {
    val in_a = Input(UInt(bw.W))
    val in_b = Input(UInt(bw.W))
    val signaling = Input(Bool())
    val out_eq = Output(UInt(1.W))
    val out_lt = Output(UInt(1.W))
    val out_gt = Output(UInt(1.W))
    val exceptionFlags = Output(UInt(5.W))
  })

  override def desiredName = s"FPCMP_${expW}_$sigW"

  val compRecFN = Module(new CompareRecFN(expW, sigW))
  compRecFN.io.a := recFNFromFN(expW, sigW, io.in_a)
  compRecFN.io.b := recFNFromFN(expW, sigW, io.in_b)
  // Signaling picks lt/le vs lt_quiet/le_quiet. testfloat tests both, so it is a port.
  compRecFN.io.signaling := io.signaling
  io.out_eq := compRecFN.io.eq
  io.out_lt := compRecFN.io.lt
  io.out_gt := compRecFN.io.gt
  io.exceptionFlags := compRecFN.io.exceptionFlags
}
