package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.planner.{step => ps}

object CostForPlanSteps {
  /** Cost of instructions operating on values in registers */
  private val trivialInstrCost = 1L
  /** Cost of a single store to memory */
  private val storeCost = 1L
  /** Cost of a single load from memory */
  private val loadCost = 1L
  /** Cost of a floating point arithmetic instruction */
  private val floatMathCost = 2L
  /** Cost of constant cell in the .data segment */
  private val constantCellCost = 2L
  /** Cost of consuming a single cell */
  private val cellConsumptionCost = 10L
  /** Cost of a garbage collector barrier */
  private val gcBarrierCost = 10L
  /** Cost of a function call */
  private val functionCallCost = 10L

  private def costForStep(step : ps.Step) : Long = step match {
    case _ : ps.DisposeValues | _ : ps.ConvertNativeInteger | _ : ps.CreateNamedEntryPoint | _ : ps.CreateBooleanCell |
         _ : ps.CreateEmptyListCell | _ : ps.CreateUnitCell | _ : ps.CastCellToTypeUnchecked =>
      // These typically don't generate any asembler
      0L
    
    case _ : ps.CreateNativeConstant =>
      // These typically will just be included in the instruction stream or as a small external constant
      trivialInstrCost

    case _ : ps.ConvertNativeFloat | _ : ps.ConvertNativeIntegerToFloat =>
      // These are usually cheap don't require a load
      trivialInstrCost

    case _ : ps.IntegerAdd | _ : ps.IntegerSub | _ : ps.IntegerMul | _ : ps.IntegerDiv | _ : ps.IntegerRem |
         _ : ps.IntegerCompare | _ : ps.FloatCompare | _ : ps.FloatIsNaN | _ : ps.FloatBitwiseCompare | _ : ps.Return =>
      // These effectively map 1:1 to assembler instructions
      trivialInstrCost

    case _ : ps.FloatAdd | _ : ps.FloatSub | _ : ps.FloatMul | _ : ps.FloatDiv =>
      floatMathCost
    
    case _ : ps.UnboxValue | _ : ps.LoadPairCar | _ : ps.LoadPairCdr | _ : ps.LoadProcedureEntryPoint |
         _ : ps.LoadVectorLength | _ : ps.LoadRecordLikeData | _ : ps.LoadRecordDataField | _ : ps.LoadVectorElement |
         _ : ps.LoadVectorElementsData | _ : ps.LoadStringCharLength | _ : ps.LoadBytevectorLength =>
      // This is a load from memory
      loadCost

    case _ : ps.SetPairCar | _ : ps.SetPairCdr | _ : ps.SetProcedureEntryPoint | _ : ps.SetRecordDataField |
         _ : ps.SetRecordLikeDefined | _ : ps.StoreVectorElement =>
      storeCost
    
    case _ : ps.TestCellType | _ : ps.TestRecordLikeClass =>
      // These are typically a load + test
      loadCost + trivialInstrCost
    
    case _ : ps.CreateConstant =>
      // Constant just create additional .data in the resulting executable and have a good chance of being merged
      constantCellCost

    case nestingStep : ps.NestingStep =>
      // The branch might force a GC barrier
      // Also, if the branch actually makes it to the assembler it can be fairly expensive
      (gcBarrierCost / 2) + trivialInstrCost +
        nestingStep.innerBranches.flatMap(_._1).map(costForStep).sum
    
    case _ : ps.CalcProperListLength =>
      // Assume lists have an average length of 5
      // We need to load the cdr and test its type for each cell
      // Add an additional instruction for dealing with looping
      (loadCost + (trivialInstrCost * 2)) * 5

    case _ : ps.InitPair | _ : ps.InitRecordLike | _ : ps.InitVector =>
      // This requires an allocation and a store to the cell type
      cellConsumptionCost

    case _ : ps.BoxValue =>
      // This requires an allocation plus a store of the boxed value
      cellConsumptionCost + storeCost

    case allocateCells : ps.AllocateCells =>
      gcBarrierCost

    case _ : ps.AssertPredicate | _ : ps.AssertPairMutable | _ : ps.AssertRecordLikeDefined =>
      // These require a test + a possible GC barrier if the test fails
      (gcBarrierCost / 2) + trivialInstrCost

    case _ : ps.LoadValueForParameterProc =>
      // This is a function call
      functionCallCost

    case _ : ps.PushDynamicState | _ : ps.PopDynamicState | _ : ps.CreateParameterProc =>
      // These are allocating function calls
      functionCallCost + gcBarrierCost

    case invoke : ps.InvokeLike =>
      functionCallCost + (if (invoke.signature.hasWorldArg) {
        // This can allocate cells and throw exceptions - this requires a GC barrier
        gcBarrierCost
      }
      else {
        // This can be invoked without a GC barrier
        0L
      })
  }

  /** Calculates a cost for a sequence of plan steps
    *
    * The number returned isn't meaningful except for ranking the relative cost of different plans
    */
  def apply(steps : Seq[ps.Step]) : Long =
    steps.map(costForStep).sum
}
