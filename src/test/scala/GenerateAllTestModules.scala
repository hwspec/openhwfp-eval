package Generate
import FloatingPoint.fpu._
import Primitives.primitives._
import hardfloat._
import rial.arith.{AddFPGeneric, MultFPGeneric, FusedMulAddFPGeneric, RealSpec, RoundSpec}
import rial.math.{MathFuncConfig, MathFuncPipelineConfig, MathFunctions, FuncKind}
import rial.util.PipelineStageConfig
import chisel3._
import circt.stage.ChiselStage
import java.io.{File, PrintWriter}
import scala.math.exp

object GenerateAllTestModules extends App {
  /** Firtool options for Yosys-friendly SystemVerilog.
    * disallowLocalVariables: no automatic logic in always blocks → fixes "invalid nesting".
    */
  private val yosysFirtoolOpts = Array(
    "--lowering-options=disallowLocalVariables",
    "--disable-all-randomization",
    "-strip-debug-info",
    "--disable-annotation-unknown"
  )

  private def genVerilog(mod: => RawModule, outputDir: String, moduleName: String): Unit = {
    val dir = new File(outputDir)
    dir.mkdirs()
    println(s"Generating: $moduleName")
    try {
      val verilog = ChiselStage.emitSystemVerilog(
        gen = mod,
        args = Array.empty,
        firtoolOpts = yosysFirtoolOpts
      )
      val outputFile = new File(outputDir, s"$moduleName.sv")
      val w = new PrintWriter(outputFile)
      w.write(verilog)
      w.close()
      println(s"  ✓ Generated: $outputDir/$moduleName.sv")
    } catch {
      case e: Exception =>
        println(s"  ✗ Failed: $moduleName - ${e.getMessage}")
        e.printStackTrace()
    }
  }

  val baseDir = "generated"
  
  println("=" * 80)
  println("Generating Verilog for ALL Test Modules")
  println("=" * 80)
  println(s"Output directory: $baseDir/")
  println()

  // ============================================================================
  // OpenFloat Modules
  // ============================================================================
  println("=" * 80)
  println("OpenFloat Modules")
  println("=" * 80)
  val openfloatDir = s"$baseDir/openfloat"
  // OpenFloatSpec (bw=32, pd=1) + OpenFloatBasicSpec iterTest (bw in 32,64; pd in 1,7,13)
  genVerilog(new FP_add(32, 1), openfloatDir, "FP_add_32_1")
  genVerilog(new FP_add(32, 7), openfloatDir, "FP_add_32_7")
  genVerilog(new FP_add(32, 13), openfloatDir, "FP_add_32_13")
  genVerilog(new FP_add(64, 1), openfloatDir, "FP_add_64_1")
  genVerilog(new FP_add(64, 7), openfloatDir, "FP_add_64_7")
  genVerilog(new FP_add(64, 13), openfloatDir, "FP_add_64_13")
  genVerilog(new FP_mult(32, 1), openfloatDir, "FP_mult_32_1")
  genVerilog(new FP_mult(32, 7), openfloatDir, "FP_mult_32_7")
  genVerilog(new FP_mult(32, 13), openfloatDir, "FP_mult_32_13")
  genVerilog(new FP_mult(64, 1), openfloatDir, "FP_mult_64_1")
  genVerilog(new FP_mult(64, 7), openfloatDir, "FP_mult_64_7")
  genVerilog(new FP_mult(64, 13), openfloatDir, "FP_mult_64_13")
  genVerilog(new FP_div(32, 15, 15), openfloatDir, "FP_divider_32_15_15")
  genVerilog(new FP_sqrt(32, 23, 23), openfloatDir, "FP_sqrt_32_23_23")
  genVerilog(new FP_cos(32, 23), openfloatDir, "FP_cos_32_23")

  // ============================================================================
  // HardFloat Modules
  // ============================================================================
  println()
  println("=" * 80)
  println("HardFloat Modules")
  println("=" * 80)
  val hardfloatDir = s"$baseDir/hardfloat"
  
  // FP32 modules
  genVerilog(new FPTest(8, 24), hardfloatDir, "FPTest_8_24")
  genVerilog(new FPOPTest(8, 24, FPOPTestMode.ADD), hardfloatDir, "FPADD_8_24")
  genVerilog(new FPOPTest(8, 24, FPOPTestMode.SUB), hardfloatDir, "FPSUB_8_24")
  genVerilog(new FPOPTest(8, 24, FPOPTestMode.MUL), hardfloatDir, "FPMUL_8_24")
  genVerilog(new FPDIVTest(8, 24), hardfloatDir, "FPDIV_8_24")
  genVerilog(new FPSqrtTest(8, 24), hardfloatDir, "FPSQRT_8_24")
  
  // FP64 modules
  genVerilog(new FPOPTest(11, 53, FPOPTestMode.ADD), hardfloatDir, "FPADD_11_53")
  genVerilog(new FPOPTest(11, 53, FPOPTestMode.SUB), hardfloatDir, "FPSUB_11_53")
  genVerilog(new FPOPTest(11, 53, FPOPTestMode.MUL), hardfloatDir, "FPMUL_11_53")
  genVerilog(new FPDIVTest(11, 53), hardfloatDir, "FPDIV_11_53")
  genVerilog(new FPSqrtTest(11, 53), hardfloatDir, "FPSQRT_11_53")

  // ============================================================================
  // Rial Modules
  // ============================================================================
  println()
  println("=" * 80)
  println("Rial Modules")
  println("=" * 80)
  val rialDir = s"$baseDir/rial"
  
  // FP16 Arithmetic
  val fp16Spec = RealSpec.Float16Spec
  genVerilog(new AddFPGeneric(fp16Spec, fp16Spec, fp16Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialAddFP16")
  genVerilog(new MultFPGeneric(fp16Spec, fp16Spec, fp16Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialMultFP16")
  genVerilog(new FusedMulAddFPGeneric(fp16Spec, fp16Spec, fp16Spec, fp16Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialFusedMulAddFP16")
  
  // FP32 Arithmetic
  val fp32Spec = RealSpec.Float32Spec
  genVerilog(new AddFPGeneric(fp32Spec, fp32Spec, fp32Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialAddFP32")
  genVerilog(new MultFPGeneric(fp32Spec, fp32Spec, fp32Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialMultFP32")
  genVerilog(new FusedMulAddFPGeneric(fp32Spec, fp32Spec, fp32Spec, fp32Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialFusedMulAddFP32")
  
  // FP64 Arithmetic
  val fp64Spec = RealSpec.Float64Spec
  genVerilog(new AddFPGeneric(fp64Spec, fp64Spec, fp64Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialAddFP64")
  genVerilog(new MultFPGeneric(fp64Spec, fp64Spec, fp64Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialMultFP64")
  genVerilog(new FusedMulAddFPGeneric(fp64Spec, fp64Spec, fp64Spec, fp64Spec, RoundSpec.roundToEven, PipelineStageConfig.none), 
              rialDir, "RialFusedMulAddFP64")
  
  // Math Functions - FP16
  val nOrderFP16 = 3
  val adrWFP16 = 8
  val extraBitsFP16 = 0
  
  // FP16 Math functions
  val fncfgFP16_sqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt)))
  genVerilog(new MathFunctions(fncfgFP16_sqrt, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16, 
                                MathFuncPipelineConfig.none), rialDir, "RialSqrtFP16")
  
  val fncfgFP16_invsqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.InvSqrt)))
  genVerilog(new MathFunctions(fncfgFP16_invsqrt, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16, 
                               MathFuncPipelineConfig.none), rialDir, "RialInvSqrtFP16")
  
  val fncfgFP16_sin = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sin)))
  genVerilog(new MathFunctions(fncfgFP16_sin, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16, 
                                MathFuncPipelineConfig.none), rialDir, "RialSinFP16")
  
  val fncfgFP16_cos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Cos)))
  genVerilog(new MathFunctions(fncfgFP16_cos, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialCosFP16")
  
  val fncfgFP16_reciprocal = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal)))
  genVerilog(new MathFunctions(fncfgFP16_reciprocal, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialReciprocalFP16")
  
  val fncfgFP16_exp = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Exp)))
  genVerilog(new MathFunctions(fncfgFP16_exp, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialExpFP16")
  
  val fncfgFP16_log = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Log)))
  genVerilog(new MathFunctions(fncfgFP16_log, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialLogFP16")
  
  val fncfgFP16_sigmoid = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sigmoid)))
  genVerilog(new MathFunctions(fncfgFP16_sigmoid, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialSigmoidFP16")
  
  val fncfgFP16_acos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt, FuncKind.ACosPhase1, FuncKind.ACosPhase2)))
  genVerilog(new MathFunctions(fncfgFP16_acos, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialAcosFP16")
  
  val fncfgFP16_softplus = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.SoftPlus)))
  genVerilog(new MathFunctions(fncfgFP16_softplus, fp16Spec, nOrderFP16, adrWFP16, 1,
                               MathFuncPipelineConfig.none), rialDir, "RialSoftPlusFP16")
  
  val fncfgFP16_smg = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.ScaleMixtureGaussian)), Some((exp(-1.0), exp(-6.0))))
  genVerilog(new MathFunctions(fncfgFP16_smg, fp16Spec, nOrderFP16, adrWFP16, extraBitsFP16,
                               MathFuncPipelineConfig.none), rialDir, "RialSMGFP16")
  
  val fncfgFP16_atan2 = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal, FuncKind.ATan2Phase1, FuncKind.ATan2Phase2)))
  genVerilog(new MathFunctions(fncfgFP16_atan2, fp16Spec, 0, 8, 0, MathFuncPipelineConfig.none), rialDir, "RialAtan2FP16")
  
  // Math Functions - FP32
  val nOrderFP32 = 3
  val adrWFP32 = 8
  val extraBitsFP32 = 0
  
  val fncfgFP32_sqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt)))
  genVerilog(new MathFunctions(fncfgFP32_sqrt, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialSqrtFP32")
  
  val fncfgFP32_invsqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.InvSqrt)))
  genVerilog(new MathFunctions(fncfgFP32_invsqrt, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialInvSqrtFP32")
  
  val fncfgFP32_sin = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sin)))
  genVerilog(new MathFunctions(fncfgFP32_sin, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialSinFP32")
  
  val fncfgFP32_cos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Cos)))
  genVerilog(new MathFunctions(fncfgFP32_cos, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialCosFP32")
  
  val fncfgFP32_exp = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Exp)))
  genVerilog(new MathFunctions(fncfgFP32_exp, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialExpFP32")
  
  val fncfgFP32_log = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Log)))
  genVerilog(new MathFunctions(fncfgFP32_log, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialLogFP32")
  
  val fncfgFP32_reciprocal = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal)))
  genVerilog(new MathFunctions(fncfgFP32_reciprocal, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32, 
                               MathFuncPipelineConfig.none), rialDir, "RialReciprocalFP32")
  
  val fncfgFP32_sigmoid = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sigmoid)))
  genVerilog(new MathFunctions(fncfgFP32_sigmoid, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32,
                               MathFuncPipelineConfig.none), rialDir, "RialSigmoidFP32")
  
  val fncfgFP32_acos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt, FuncKind.ACosPhase1, FuncKind.ACosPhase2)))
  genVerilog(new MathFunctions(fncfgFP32_acos, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32,
                               MathFuncPipelineConfig.none), rialDir, "RialAcosFP32")
  
  val fncfgFP32_softplus = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.SoftPlus)))
  genVerilog(new MathFunctions(fncfgFP32_softplus, fp32Spec, nOrderFP32, adrWFP32, 1,
                               MathFuncPipelineConfig.none), rialDir, "RialSoftPlusFP32")
  
  val fncfgFP32_smg = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.ScaleMixtureGaussian)), Some((exp(-1.0), exp(-6.0))))
  genVerilog(new MathFunctions(fncfgFP32_smg, fp32Spec, nOrderFP32, adrWFP32, extraBitsFP32,
                               MathFuncPipelineConfig.none), rialDir, "RialSMGFP32")
  
  val fncfgFP32_atan2 = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal, FuncKind.ATan2Phase1, FuncKind.ATan2Phase2)))
  genVerilog(new MathFunctions(fncfgFP32_atan2, fp32Spec, 0, 8, 0, MathFuncPipelineConfig.none), rialDir, "RialAtan2FP32")
  
  // Math Functions - FP64
  val nOrderFP64 = 3
  val adrWFP64 = 8
  val extraBitsFP64 = 0
  
  val fncfgFP64_sqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt)))
  genVerilog(new MathFunctions(fncfgFP64_sqrt, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64, 
                               MathFuncPipelineConfig.none), rialDir, "RialSqrtFP64")
  
  val fncfgFP64_invsqrt = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.InvSqrt)))
  genVerilog(new MathFunctions(fncfgFP64_invsqrt, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64, 
                               MathFuncPipelineConfig.none), rialDir, "RialInvSqrtFP64")
  
  val fncfgFP64_exp = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Exp)))
  genVerilog(new MathFunctions(fncfgFP64_exp, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64, 
                               MathFuncPipelineConfig.none), rialDir, "RialExpFP64")
  
  val fncfgFP64_log = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Log)))
  genVerilog(new MathFunctions(fncfgFP64_log, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64, 
                               MathFuncPipelineConfig.none), rialDir, "RialLogFP64")
  
  val fncfgFP64_sigmoid = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sigmoid)))
  genVerilog(new MathFunctions(fncfgFP64_sigmoid, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialSigmoidFP64")
  
  val fncfgFP64_reciprocal = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal)))
  genVerilog(new MathFunctions(fncfgFP64_reciprocal, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialReciprocalFP64")
  
  val fncfgFP64_acos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sqrt, FuncKind.ACosPhase1, FuncKind.ACosPhase2)))
  genVerilog(new MathFunctions(fncfgFP64_acos, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialAcosFP64")
  
  val fncfgFP64_sin = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Sin)))
  genVerilog(new MathFunctions(fncfgFP64_sin, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialSinFP64")
  
  val fncfgFP64_cos = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Cos)))
  genVerilog(new MathFunctions(fncfgFP64_cos, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialCosFP64")
  
  val fncfgFP64_softplus = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.SoftPlus)))
  genVerilog(new MathFunctions(fncfgFP64_softplus, fp64Spec, nOrderFP64, adrWFP64, 1,
                               MathFuncPipelineConfig.none), rialDir, "RialSoftPlusFP64")
  
  val fncfgFP64_smg = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.ScaleMixtureGaussian)), Some((exp(-1.0), exp(-6.0))))
  genVerilog(new MathFunctions(fncfgFP64_smg, fp64Spec, nOrderFP64, adrWFP64, extraBitsFP64,
                               MathFuncPipelineConfig.none), rialDir, "RialSMGFP64")
  
  val fncfgFP64_atan2 = new MathFuncConfig(FuncKind.normalize(Seq(FuncKind.Reciprocal, FuncKind.ATan2Phase1, FuncKind.ATan2Phase2)))
  genVerilog(new MathFunctions(fncfgFP64_atan2, fp64Spec, 0, 8, 0, MathFuncPipelineConfig.none), rialDir, "RialAtan2FP64")
  
  println()
  println("=" * 80)
  println("Verilog generation complete!")
  println("=" * 80)
  println(s"All files generated in: $baseDir/")
  println(s"  - OpenFloat: $openfloatDir/ (ADD/SUB/MULT/Div/Sqrt/Cos at configs used by 60 tests)")
  println(s"  - HardFloat: $hardfloatDir/")
  println(s"  - Rial: $rialDir/ (all FP16/32/64 ops: Add, Mult, FMA, Sqrt, InvSqrt, Sin, Cos, Exp, Log, Reciprocal, Sigmoid, Acos, SoftPlus, SMG, Atan2)")
  println("Run these .sv files through Yosys for PPA/synthesis.")
  println("=" * 80)
}
