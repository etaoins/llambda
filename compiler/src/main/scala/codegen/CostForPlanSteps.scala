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
  /** Cost of consuming a single cell from the stack */
  private val stackCellConsumptionCost = 5L
  /** Cost of consuming a single cell from the GC heap */
  private val heapCellConsumptionCost = 10L
  /** Cost of a function call */
  private val functionCallCost = 10L

  private def allocCostForStackAllocable(step: ps.StackAllocableStep) =
    if (step.stackAllocate) stackCellConsumptionCost else heapCellConsumptionCost

  private def costForStep(step: ps.Step): Long = step match {
    case _: ps.ConvertNativeInteger | _: ps.CreateNamedEntryPoint | _: ps.CreateBooleanCell |
         _: ps.CreateEmptyListCell | _: ps.CreateUnitCell | _: ps.CastCellToTypeUnchecked =>
      // These typically don't generate any assembler
      0L

    case _: ps.CreateNativeConstant =>
      // These typically will just be included in the instruction stream or as a small external constant
      trivialInstrCost

    case _: ps.ConvertNativeFloat | _: ps.ConvertNativeIntegerToFloat =>
      // These are usually cheap don't require a load
      trivialInstrCost

    case _: ps.IntegerDiv | _: ps.IntegerRem | _: ps.IntegerCompare | _: ps.FloatCompare | _: ps.FloatIsNaN |
         _: ps.FloatBitwiseCompare | _: ps.Return =>
      // These effectively map 1:1 to assembler instructions
      trivialInstrCost

    case _: ps.CheckedIntegerStep =>
      // These are a trivial instructions + a very predicable branch to hit overflow
      trivialInstrCost * 2

    case _: ps.FloatAdd | _: ps.FloatSub | _: ps.FloatMul | _: ps.FloatDiv =>
      floatMathCost

    case _: ps.UnboxValue | _: ps.LoadPairCar | _: ps.LoadPairCdr | _: ps.LoadProcedureEntryPoint |
         _: ps.LoadVectorLength |_: ps.LoadVectorElement | _: ps.LoadBytevectorLength | _: ps.LoadSymbolByteLength |
         _: ps.LoadSymbolByte =>
      // This is a load from memory
      loadCost

    case _: ps.SetRecordLikeDefined | _: ps.StoreVectorElement =>
      storeCost

    case ps.LoadRecordLikeFields(_, _, fieldsToLoad) =>
      // Load for the record data pointer + each field
      loadCost + (loadCost * fieldsToLoad.length)

    case ps.SetRecordLikeFields(_, _, fieldsToSet) =>
      // Load for the record data pointer + each field
      loadCost + (storeCost * fieldsToSet.length)

    case _: ps.TestCellType | _: ps.TestRecordLikeClass =>
      // These are typically a load + test
      loadCost + trivialInstrCost

    case _: ps.CreateConstant =>
      // Constant just create additional .data in the resulting executable and have a good chance of being merged
      constantCellCost

    case condBranch: ps.CondBranch =>
      trivialInstrCost +
        condBranch.innerBranches.flatMap(_._1).map(costForStep).sum

    case _: ps.CalcProperListLength =>
      // Assume lists have an average length of 5
      // We need to load the cdr and test its type for each cell
      // Add an additional instruction for dealing with looping
      (loadCost + (trivialInstrCost * 2)) * 5

    case initPair: ps.InitPair =>
      // This requires an allocation and a store to the car and cdr
      val allocCost = allocCostForStackAllocable(initPair)
      allocCost + (storeCost * 2)

    case ps.InitVector(_, elements) =>
      heapCellConsumptionCost + (elements.length * storeCost)

    case _: ps.InitFilledVector =>
      // Assume the average vector is 8 elements long
      heapCellConsumptionCost + (8 * storeCost)

    case initRecordLike: ps.InitRecordLikeStep =>
      // This requires an allocation and then a store for each field
      heapCellConsumptionCost + (initRecordLike.fieldValues.size * storeCost)

    case _: ps.BoxBoolean =>
      // This selects a constant value based on a predicate
      trivialInstrCost

    case allocating: ps.AllocatingBoxValue =>
      // This requires an allocation plus a store of the boxed value
      val allocCost = allocCostForStackAllocable(allocating)
      allocCost + storeCost

    case _: ps.AllocateHeapCells | _: ps.AssertPredicate | _: ps.AssertRecordLikeDefined =>
      // These require a test
      trivialInstrCost

    case _: ps.LoadValueForParameterProc | _: ps.PushDynamicState | _: ps.PopDynamicState  | _: ps.CreateParameterProc |
         _: ps.InvokeLike  =>
      // These are a function call
      functionCallCost
  }

  /** Calculates a cost for a sequence of plan steps
    *
    * The number returned isn't meaningful except for ranking the relative cost of different plans
    */
  def apply(steps: Seq[ps.Step]): Long =
    steps.map(costForStep).sum
}
