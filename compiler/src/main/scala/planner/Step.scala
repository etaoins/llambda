package io.llambda.compiler.planner.step
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated, RuntimeErrorMessage}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{AdapterProcType, AdapterProcField}


class TempValue {
  override def toString = s"%${this.hashCode.toHexString}"
}

object TempValue {
  def apply() = new TempValue
}

object WorldPtrValue extends TempValue


sealed trait Step extends ContextLocated {
  val inputValues: Set[TempValue]
  val outputValues: Set[TempValue]

  /** Renames all the temp values this step references
    *
    * This is used to remove redundant values in the conniver
    */
  def renamed(f: (TempValue) => TempValue): Step

  /** Indicates a step that can throw an exception
    *
    * This means the heap state has to be fully in sync - we can have no allocated but uninitialized conses, etc.
    */
  def canThrow: Boolean = false

  /** Indicates if this step can be discarded if its output values are unused */
  def discardable: Boolean = false

  /** Indicates the number of heap allocated cells this step requires */
  def requiredHeapCells: Int = 0
}

/** Step producing a values that can be discarded if its output values are unused */
sealed trait DiscardableStep extends Step {
  override def discardable = true
}

/** Step producing a value that can be discarded or merged with identical instances of itself
  *
  * These must satisfy the following properties:
  * - The step must not depend on global state
  * - The step must not have any side effects on global state besides potentially raising an error
  * - The source and destination values must either both be immutable or reference the same location in memory
  *
  * These are potentially merged by conniver.MergeIdenticalSteps
  */
sealed trait MergeableStep extends Step {
  val result: TempValue

  /** Key used to compare if two mergeable steps are the same
    *
    * The default key is just the original mergeable step with the result renamed to a constant value. Other steps may
    * want to override this return a tuple of fields to exclude things like error messages.
    */
   def mergeKey: Any = {
     this.renamed({ tempValue =>
       if (outputValues.contains(tempValue)) {
         MergeableStep.PlaceholderResultTemp
       }
       else {
         tempValue
       }
     })
   }
}

object MergeableStep {
  object PlaceholderResultTemp extends TempValue
}

/** Step that can conditionally terminate execution based on a test */
sealed trait AssertStep extends MergeableStep {
  val result = MergeableStep.PlaceholderResultTemp
}

/** Step that can be converted to using stack allocations for uncaptured values */
sealed trait StackAllocableStep extends Step {
  val result: TempValue
  val stackAllocate: Boolean

  def asStackAllocated: StackAllocableStep
}

/** Step using a record-like type
  *
  * This is used by codegen to find all referenced types from a given plan
  */
sealed trait RecordLikeStep extends Step {
  val recordLikeType: vt.RecordLikeType
}

sealed trait InvokeLike extends Step {
  val signature: ProcedureSignature

  // The world arg is required to throw an exception
  override def canThrow = signature.hasWorldArg
}

/** Invokes an entry point with the given arguments
  *
  * Entry points can be loaded with CreateNamedEntryPoint
  */
case class Invoke(
    result: Option[TempValue],
    signature: ProcedureSignature,
    entryPoint: TempValue,
    arguments: List[TempValue],
    override val discardable: Boolean = false
) extends InvokeLike {
  lazy val inputValues = arguments.toSet + entryPoint
  lazy val outputValues = result.toSet

  def renamed(f: (TempValue) => TempValue) =
    Invoke(
      result=result.map(f),
      signature=signature,
      entryPoint=f(entryPoint),
      arguments=arguments.map(f),
      discardable=discardable
    ).assignLocationFrom(this)
}

/** Invokes a procedure and immediately returns the value */
case class TailCall(signature: ProcedureSignature, entryPoint: TempValue, arguments: List[TempValue]) extends InvokeLike {
  lazy val inputValues = arguments.toSet + entryPoint
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    TailCall(
      signature=signature,
      entryPoint=f(entryPoint),
      arguments=arguments.map(f)
    ).assignLocationFrom(this)
}

/** Allocates a given number of cells from the GC heap at runtime
 *
 * This should only be inserted in to the plan by PlanHeapAllocations
 */
case class AllocateHeapCells(count: Int) extends Step {
  val inputValues = Set[TempValue](WorldPtrValue)
  val outputValues = Set[TempValue]()

  override def canThrow = true

  def renamed(f: (TempValue) => TempValue) = this
}

/** Represents a value to be phi'ed by a branch
  *
  * @param result      location to store trueValue or falseValue when the branch completes
  * @param trueValue   value to place in result after performing trueSteps
  * @param falseValue  value to place in result after performing falseSteps
  */
case class ValuePhi(result: TempValue, trueValue: TempValue, falseValue: TempValue) {
  def renamed(f: (TempValue) => TempValue) =
    ValuePhi(f(result), f(trueValue), f(falseValue))
}

/** Conditionally branches based on a value
  *
  * @param test        i1 value to conditionally branch on
  * @param trueSteps   steps to perform if the condition is true
  * @param falseSteps  steps to perform if the condition is false
  * @param valuePhis    values to phi from the branches
  */
case class CondBranch(test: TempValue, trueSteps: List[Step], falseSteps: List[Step], valuePhis: List[ValuePhi]) extends Step {
  lazy val outerInputValues = Set(test)
  lazy val innerBranches = List(
    (trueSteps, valuePhis.map(_.trueValue).toSet),
    (falseSteps, valuePhis.map(_.falseValue).toSet)
  )

  lazy val outputValues = valuePhis.map(_.result).toSet

  lazy val inputValues =
    outerInputValues ++
    innerBranches.flatMap(_._1).flatMap(_.inputValues) ++
    innerBranches.flatMap(_._2)

  def mapInnerBranches(mapper: (List[Step], List[TempValue]) => (List[Step], List[TempValue])) = {
    val (mappedTrueSteps, mappedTrueValues) = mapper(trueSteps, valuePhis.map(_.trueValue))
    val (mappedFalseSteps, mappedFalseValues) = mapper(falseSteps, valuePhis.map(_.falseValue))

    val mappedPhis = (valuePhis.map(_.result), mappedTrueValues, mappedFalseValues).zipped.map {
      case (result, trueValue, falseValue) => ValuePhi(result, trueValue, falseValue)
    }

    CondBranch(test, mappedTrueSteps, mappedFalseSteps, mappedPhis).assignLocationFrom(this)
  }

  def renamed(f: (TempValue) => TempValue) =
    CondBranch(
      test=f(test),
      trueSteps=trueSteps.map(_.renamed(f)),
      falseSteps=falseSteps.map(_.renamed(f)),
      valuePhis=valuePhis.map(_.renamed(f))
    ).assignLocationFrom(this)
}

/** Tests if a cell is of a given type */
case class TestCellType(
    result: TempValue,
    value: TempValue,
    testType: ct.ConcreteCellType,
    possibleTypes: Set[ct.ConcreteCellType] = ct.AnyCell.concreteTypes
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    TestCellType(f(result), f(value), testType, possibleTypes).assignLocationFrom(this)

  private case class MergeKey(value: TempValue, testType: ct.ConcreteCellType)

  override def mergeKey: Any =
    // Merge steps with different possibleTypes - it's just an optimisation hint
    MergeKey(value, testType)
}

/** Casts a cell to another type without checking the validity of the cast */
case class CastCellToTypeUnchecked(
    result: TempValue,
    value: TempValue,
    toType: ct.CellType
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    CastCellToTypeUnchecked(f(result), f(value), toType).assignLocationFrom(this)
}

/** Converts an native integer to another width and/or signedness */
case class ConvertNativeInteger(
    result: TempValue,
    fromValue: TempValue,
    toBits: Int,
    signed: Boolean
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    ConvertNativeInteger(f(result), f(fromValue), toBits, signed).assignLocationFrom(this)
}

/** Converts an native float to another type */
case class ConvertNativeFloat(
    result: TempValue,
    fromValue: TempValue,
    toType: vt.FpType
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    ConvertNativeFloat(f(result), f(fromValue), toType).assignLocationFrom(this)
}

/** Converts an native integer to a float */
case class ConvertNativeIntegerToFloat(
    result: TempValue,
    fromValue: TempValue,
    fromSigned: Boolean,
    toType: vt.FpType
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    ConvertNativeIntegerToFloat(f(result), f(fromValue), fromSigned, toType).assignLocationFrom(this)
}

/** Calculates the length of a proper list as a uint32
  *
  * The passed list must be a proper list or the result is undefined
  */
case class CalcProperListLength(
    result: TempValue,
    listHead: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(listHead)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    CalcProperListLength(f(result), f(listHead)).assignLocationFrom(this)
}

/** Indicates a step that creates a constant value */
sealed trait CreateConstant extends DiscardableStep with MergeableStep {
  val result: TempValue
  lazy val outputValues = Set(result)
}

/** Creates an entry point with the given signature and native symbol
  *
  * This can be called with Invoke
  */
case class CreateNamedEntryPoint(
    result: TempValue,
    signature: ProcedureSignature,
    nativeSymbol: String
) extends DiscardableStep with MergeableStep {
  val inputValues = Set[TempValue]()
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    CreateNamedEntryPoint(f(result), signature, nativeSymbol).assignLocationFrom(this)
}

/** Indicates a step that creates a constant cell */
sealed trait CreateConstantCell extends CreateConstant

case class CreateStringCell(result: TempValue, value: String) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateStringCell(f(result), value).assignLocationFrom(this)
}

case class CreateSymbolCell(result: TempValue, value: String) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateSymbolCell(f(result), value).assignLocationFrom(this)
}

case class CreateIntegerCell(result: TempValue, value: Long) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateIntegerCell(f(result), value).assignLocationFrom(this)
}

case class CreateFlonumCell(result: TempValue, value: Double) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateFlonumCell(f(result), value).assignLocationFrom(this)

  private case class MergeKey(value: Double)

  override def mergeKey: Any =
    // Use the long bits so +nan.0, -0.0 etc are handled correctly
    MergeKey(java.lang.Double.doubleToLongBits(value))
}

case class CreateCharCell(result: TempValue, value: Int) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateCharCell(f(result), value).assignLocationFrom(this)
}

case class CreateBooleanCell(result: TempValue, value: Boolean) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateBooleanCell(f(result), value).assignLocationFrom(this)
}

case class CreateBytevectorCell(result: TempValue, elements: Vector[Short]) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateBytevectorCell(f(result), elements).assignLocationFrom(this)
}

case class CreatePairCell(result: TempValue, car: TempValue, cdr: TempValue, listLengthOpt: Option[Long]) extends CreateConstantCell {
  lazy val inputValues = Set(car, cdr)

  def renamed(f: (TempValue) => TempValue) =
    CreatePairCell(f(result), f(car), f(cdr), listLengthOpt).assignLocationFrom(this)
}

case class CreateVectorCell(result: TempValue, elements: Vector[TempValue]) extends CreateConstantCell {
  lazy val inputValues = elements.toSet

  def renamed(f: (TempValue) => TempValue) =
    CreateVectorCell(f(result), elements.map(f)).assignLocationFrom(this)
}

case class CreateUnitCell(result: TempValue) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateUnitCell(f(result)).assignLocationFrom(this)
}

case class CreateEmptyListCell(result: TempValue) extends CreateConstantCell {
  val inputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    CreateEmptyListCell(f(result)).assignLocationFrom(this)
}

/** Creates a procedure with an empty closure
  *
  * This is equivalent to RecordLikeInit(vt.EmptyClosureType] except it uses compile time constant cell and is
  * considerably more efficient
  **/
case class CreateEmptyClosure(result: TempValue, entryPoint: TempValue) extends CreateConstantCell {
  lazy val inputValues = Set(entryPoint)

  def renamed(f: (TempValue) => TempValue) =
    CreateEmptyClosure(f(result), f(entryPoint)).assignLocationFrom(this)
}

/** Creates a constant record cell
  *
  * @param  recordType   Record type to create
  * @param  fieldValues  Values for the record's fields. All field values must be constants. Additionally, the record
  *                      type should not contain mutable fields if the record can be mutated by Scheme code
  * @param  isUndefined  Indicates if the record should be initially marked as undefined
  */
case class CreateRecordCell(
    result: TempValue,
    recordType: vt.RecordType,
    fieldValues: Map[vt.RecordField, TempValue],
    isUndefined: Boolean
) extends CreateConstantCell with RecordLikeStep {
  lazy val inputValues = fieldValues.values.toSet

  val recordLikeType = recordType

  def renamed(f: (TempValue) => TempValue) =
    CreateRecordCell(f(result), recordType, fieldValues.mapValues(f), isUndefined).assignLocationFrom(this)
}

/** Indicates a step that creates a native constant */
sealed trait CreateNativeConstant extends CreateConstant {
  val inputValues = Set[TempValue]()
}

case class CreateNativeInteger(result: TempValue, value: Long, bits: Int) extends CreateNativeConstant {
  def renamed(f: (TempValue) => TempValue) =
    CreateNativeInteger(f(result), value, bits).assignLocationFrom(this)
}

case class CreateNativeFloat(result: TempValue, value: Double, fpType: vt.FpType) extends CreateNativeConstant {
  def renamed(f: (TempValue) => TempValue) =
    CreateNativeFloat(f(result), value, fpType).assignLocationFrom(this)

  private case class MergeKey(value: Double, fpType: vt.FpType)

  override def mergeKey: Any =
    // Use the long bits so +nan.0, -0.0 etc are handled correctly
    MergeKey(java.lang.Double.doubleToLongBits(value), fpType)
}

/** Indicates a step that unboxes a cell */
sealed trait UnboxValue extends DiscardableStep with MergeableStep {
  val result: TempValue
  val boxed: TempValue

  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

case class UnboxInteger(result: TempValue, boxed: TempValue) extends UnboxValue {
  def renamed(f: (TempValue) => TempValue) =
    UnboxInteger(f(result), f(boxed)).assignLocationFrom(this)
}

case class UnboxFlonum(result: TempValue, boxed: TempValue) extends UnboxValue {
  def renamed(f: (TempValue) => TempValue) =
    UnboxFlonum(f(result), f(boxed)).assignLocationFrom(this)
}

case class UnboxChar(result: TempValue, boxed: TempValue) extends UnboxValue {
  def renamed(f: (TempValue) => TempValue) =
    UnboxChar(f(result), f(boxed)).assignLocationFrom(this)
}

// These aren't quite an unboxing because there's two values per boxed value

sealed trait LoadPairValue extends DiscardableStep with MergeableStep {
  val result: TempValue
  val boxed: TempValue

  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Loads the car of the passed PairCell as a AnyCell */
case class LoadPairCar(result: TempValue, boxed: TempValue) extends LoadPairValue {
  def renamed(f: (TempValue) => TempValue) =
    LoadPairCar(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads the cdr of the passed PairCell as a AnyCell */
case class LoadPairCdr(result: TempValue, boxed: TempValue) extends LoadPairValue {
  def renamed(f: (TempValue) => TempValue) =
    LoadPairCdr(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads the entry point of a procedure */
case class LoadProcedureEntryPoint(
    result: TempValue,
    boxed: TempValue,
    signature: ProcedureSignature
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadProcedureEntryPoint(f(result), f(boxed), signature)
      .assignLocationFrom(this)
}

/** Loads the length of a symbol in bytes as a UInt32
  *
  * This is nullipotent as a symbol's byte length is immutable
  */
case class LoadSymbolByteLength(
    result: TempValue,
    boxed: TempValue,
    possibleLengthsOpt: Option[Set[Int]] = None
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadSymbolByteLength(f(result), f(boxed), possibleLengthsOpt)
      .assignLocationFrom(this)
}

/** Loads a byte from a symbol
  *
  * This requires that the symbol has a statically known length in bytes
  *
  * @param  result            Result value as a UInt8
  * @param  boxed             Boxed symbol value
  * @param  offset            Requested offset in UInt32 bytes
  * @param  symbolByteLength  Statically known length of the symbol in bytes. This is used to handle inline symbols.
  */
case class LoadSymbolByte(
    result: TempValue,
    boxed: TempValue,
    offset: TempValue,
    symbolByteLength: Long,
    possibleValuesOpt: Option[Set[Byte]] = None
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(boxed, offset)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadSymbolByte(f(result), f(boxed), f(offset), symbolByteLength, possibleValuesOpt)
      .assignLocationFrom(this)
}

/** Loads the length of a bytevector as an Int64 */
case class LoadBytevectorLength(
    result: TempValue,
    boxed: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadBytevectorLength(f(result), f(boxed)).assignLocationFrom(this)
}

/** Creates a new dynamically allocated vector with specific elements
  *
  * @param  result    Result value as VectorCell
  * @param  elements  Elements to initialise the vector with as AnyCell
  */
case class InitVector(
    result: TempValue,
    elements: Vector[TempValue]
) extends DiscardableStep {
  lazy val inputValues = elements.toSet + WorldPtrValue
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    InitVector(f(result), elements.map(f)).assignLocationFrom(this)

  override def canThrow: Boolean =
    true
}

/** Creates a new dynamically allocated vector with a fill value
  *
  * @param  result  Result value as VectorCell
  * @param  length  Length of the new vector as Int64
  * @param  fill    Value to fill the vector with as AnyCell
  */
case class InitFilledVector(
    result: TempValue,
    length: TempValue,
    fill: TempValue
) extends DiscardableStep {
  lazy val inputValues = Set(length, fill, WorldPtrValue)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    InitFilledVector(f(result), f(length), f(fill)).assignLocationFrom(this)

  override def canThrow: Boolean =
    true
}

/** Loads the length of a vector as an Int64 */
case class LoadVectorLength(
    result: TempValue,
    boxed: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadVectorLength(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads an element from a vector
  *
  * @param  vectorCell  Vector to load an element from
  * @param  index       Index of the element to load as an Int64. This value must be determined to be in range
  */
case class LoadVectorElement(
    result: TempValue,
    vectorCell: TempValue,
    index: TempValue
) extends Step with DiscardableStep {
  lazy val inputValues = Set(vectorCell, index)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadVectorElement(f(result), f(vectorCell), f(index)).assignLocationFrom(this)
}

/** Store an element in a vector
  *
  * @param  vectorCell  Vector to load an element from
  * @param  index       Index of the element to load as a Int64. This value must be determined to be in range
  * @param  newValue    Boxed value to store at the element index
  */
case class StoreVectorElement(
    vectorCell: TempValue,
    index: TempValue,
    newValue: TempValue
) extends Step {
  lazy val inputValues = Set(vectorCell, index, newValue)
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    StoreVectorElement(f(vectorCell), f(index), f(newValue)).assignLocationFrom(this)
}

/** Indicates a step that boxes a native value
  *
  * These are mergeable because SSA guarantees native values can't change at runtime
  */
sealed trait BoxValue extends DiscardableStep with MergeableStep {
  val result: TempValue
  val unboxed: TempValue

  lazy val outputValues = Set(result)
}

sealed trait AllocatingBoxValue extends BoxValue with StackAllocableStep

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result: TempValue, unboxed: TempValue) extends BoxValue {
  lazy val inputValues = Set(unboxed)

  def renamed(f: (TempValue) => TempValue) =
    BoxBoolean(f(result), f(unboxed)).assignLocationFrom(this)
}

case class BoxInteger(
    result: TempValue,
    unboxed: TempValue,
    stackAllocate: Boolean = false
) extends AllocatingBoxValue {
  lazy val inputValues = Set(unboxed)
  override def requiredHeapCells = if (stackAllocate) 0 else 1

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    BoxInteger(f(result), f(unboxed), stackAllocate).assignLocationFrom(this)
}

case class BoxFlonum(
    result: TempValue,
    unboxed: TempValue,
    stackAllocate: Boolean = false
) extends AllocatingBoxValue {
  lazy val inputValues = Set(unboxed)
  override def requiredHeapCells = if (stackAllocate) 0 else 1

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    BoxFlonum(f(result), f(unboxed), stackAllocate).assignLocationFrom(this)
}

case class BoxChar(
    result: TempValue,
    unboxed: TempValue,
    stackAllocate: Boolean = false
) extends AllocatingBoxValue {
  lazy val inputValues = Set(unboxed)
  override def requiredHeapCells = if (stackAllocate) 0 else 1

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    BoxChar(f(result), f(unboxed), stackAllocate).assignLocationFrom(this)
}

/** Returns from the current function */
case class Return(returnValue: Option[TempValue]) extends Step {
  lazy val inputValues = returnValue.toSet
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    Return(returnValue.map(f)).assignLocationFrom(this)
}

/** Initialises a new pair with a defined car and cdr */
case class InitPair(
    result: TempValue,
    carValue: TempValue,
    cdrValue: TempValue,
    listLengthOpt: Option[Long] = None,
    stackAllocate: Boolean = false
) extends DiscardableStep with MergeableStep with StackAllocableStep {
  lazy val inputValues = Set(carValue, cdrValue)
  lazy val outputValues = Set(result)
  override def requiredHeapCells = if (stackAllocate) 0 else 1

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    InitPair(f(result), f(carValue), f(cdrValue), listLengthOpt, stackAllocate).assignLocationFrom(this)
}

/** Step creating a record-like cell */
sealed trait InitRecordLikeStep extends RecordLikeStep with DiscardableStep {
  def stackAllocate: Boolean
  override def requiredHeapCells = if (stackAllocate) 0 else 1

  /** Resulting record-like cell */
  val result: TempValue

  /** Record-like type being constructed */
  val recordLikeType: vt.RecordLikeType

  /** Initial values for the record-like's fields */
  val fieldValues: Map[vt.RecordField, TempValue]

  /** Indicates if the record should be initially marked as undefined. This can be tested with
    * AssertRecordLikeDefined. This can be used to implemented recursive values.
    */
  val isUndefined: Boolean

  def asStackAllocated: InitRecordLikeStep
}

/** Allocates a cell for a record of a given type
  *
  * Currently codegen can only stack allocate a record in certain situations. For this reason it's not a StackAllocable
  * and stack allocation is explicitly handled in AnalyseEscapes.
  */
case class InitRecord(
    result: TempValue,
    recordLikeType: vt.RecordType,
    fieldValues: Map[vt.RecordField, TempValue],
    isUndefined: Boolean,
    stackAllocate: Boolean = false
) extends InitRecordLikeStep {
  lazy val inputValues = fieldValues.map(_._2).toSet
  lazy val outputValues = Set(result)

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    InitRecord(f(result), recordLikeType, fieldValues.mapValues(f), isUndefined, stackAllocate).assignLocationFrom(this)
}

/** Allocates a cell for a procedure with the given closure type and entry point
  *
  * @param  entryPoint  Entry point for the procedure cell. This can be constructed with CreateNamedEntryPoint.
  */
case class InitInternalProc(
    result: TempValue,
    recordLikeType: vt.ClosureType,
    fieldValues: Map[vt.RecordField, TempValue],
    stackAllocate: Boolean = false
) extends InitRecordLikeStep {
  lazy val inputValues = fieldValues.map(_._2).toSet
  lazy val outputValues = Set(result)

  val isUndefined = false

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    InitInternalProc(f(result), recordLikeType, fieldValues.mapValues(f), stackAllocate).assignLocationFrom(this)
}

case class InitAdapterProc(
    result: TempValue,
    entryPoint: TempValue,
    adaptedClosure: TempValue,
    stackAllocate: Boolean = false
) extends InitRecordLikeStep {
  lazy val inputValues = Set(adaptedClosure, entryPoint)
  lazy val outputValues = Set(result)

  val isUndefined = false

  val recordLikeType = AdapterProcType
  val fieldValues = Map(AdapterProcField -> adaptedClosure)

  def asStackAllocated = this.copy(stackAllocate=true).assignLocationFrom(this)

  def renamed(f: (TempValue) => TempValue) =
    InitAdapterProc(f(result), f(entryPoint), f(adaptedClosure), stackAllocate).assignLocationFrom(this)
}

/** Sets a record as defined */
case class SetRecordLikeDefined(record: TempValue, recordLikeType: vt.RecordLikeType) extends RecordLikeStep {
  lazy val inputValues = Set(record)
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    SetRecordLikeDefined(f(record), recordLikeType).assignLocationFrom(this)
}

/** Asserts that a record is defined */
case class AssertRecordLikeDefined(
    record: TempValue,
    recordLikeType: vt.RecordLikeType,
    errorMessage: RuntimeErrorMessage
) extends RecordLikeStep with AssertStep {
  lazy val inputValues = Set(WorldPtrValue, record)
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    AssertRecordLikeDefined(f(record), recordLikeType, errorMessage).assignLocationFrom(this)
}

/** Sets record fields. The value must match the type of record field */
case class SetRecordLikeFields(
    record: TempValue,
    recordLikeType: vt.RecordLikeType,
    fieldsToSet: List[(TempValue, vt.RecordField)]
) extends RecordLikeStep {
  lazy val inputValues = fieldsToSet.map(_._1).toSet + record
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) = {
    val renamedFieldsToSet = fieldsToSet.map { case (newValue, field) =>
      (f(newValue), field)
    }

    SetRecordLikeFields(f(record), recordLikeType, renamedFieldsToSet).assignLocationFrom(this)
  }
}

/** Reads record fields. The value must match the type of record field */
case class LoadRecordLikeFields(
    record: TempValue,
    recordLikeType: vt.RecordLikeType,
    fieldsToLoad: List[(vt.RecordField, TempValue)]
) extends RecordLikeStep with DiscardableStep {
  lazy val inputValues = Set(record)
  lazy val outputValues = fieldsToLoad.map(_._2).toSet

  def renamed(f: (TempValue) => TempValue) = {
    val renamedFieldsToLoad = fieldsToLoad.map { case (field, result) =>
      (field, f(result))
    }

    LoadRecordLikeFields(f(record), recordLikeType, renamedFieldsToLoad).assignLocationFrom(this)
  }
}

/** Tests to see if a record is of a given class */
case class TestRecordLikeClass(
    result: TempValue,
    record: TempValue,
    recordLikeType: vt.RecordLikeType,
    possibleTypesOpt: Option[Set[vt.RecordLikeType]] = None
) extends RecordLikeStep with DiscardableStep with MergeableStep {
  lazy val inputValues = Set(record)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    TestRecordLikeClass(result, f(record), recordLikeType, possibleTypesOpt).assignLocationFrom(this)
}

/** Creates a new parameter procedure
  *
  * @param  result            Location to store the newly allocated ProcedureCell in
  * @param  initialValue      Initial value of the parameter procedure.
  */
case class CreateParameterProc(
    result: TempValue,
    initialValue: TempValue
) extends Step with DiscardableStep {
  lazy val inputValues = Set(WorldPtrValue, initialValue)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    CreateParameterProc(
      f(result),
      f(initialValue)
    ).assignLocationFrom(this)
}

/** Loads the current value of a parameter procedure
  *
  * This equivalent to applying the parameter procedure but can be implemented more efficiently.
  *
  * @param  result         Location to store the AnyCell value for the parameter procedure
  * @param  parameterProc  Parameter procdure to load the value for. The result of passing another procedure here is
  *                        undefined.
  */
case class LoadValueForParameterProc(
    result: TempValue,
    parameterProc: TempValue
) extends Step {
  lazy val inputValues = Set(WorldPtrValue, parameterProc)
  lazy val outputValues = Set(result)

  def renamed(f: (TempValue) => TempValue) =
    LoadValueForParameterProc(f(result), f(parameterProc)).assignLocationFrom(this)
}

/** Represents a value to be parameterized by PushDynamicSate
  *
  * @param  parameterProc         Parameter procedure to set the value for. A runtime error will be signalled if this is
  *                               another type of procedure
  * @param  newValue              New value for the parameter procedure. This will be automatically converted if the
  *                               was created with a converter procedure
  */
case class ParameterizedValue(
    parameterProc: TempValue,
    newValue: TempValue
) {
  def renamed(f: TempValue => TempValue) =
    ParameterizedValue(f(parameterProc), f(newValue))
}

/** Pushes a new dynamic state with the given parameter values
  *
  * @param parameterValues  Map of parameter procedure IR values to the new value the parameter should take
  */
case class PushDynamicState(parameterValues: List[ParameterizedValue]) extends Step {
  lazy val inputValues = parameterValues.flatMap({
    case ParameterizedValue(parameterProc, newValue) =>
      List(parameterProc, newValue)
  }).toSet + WorldPtrValue

  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    PushDynamicState(parameterValues.map(_.renamed(f)))
      .assignLocationFrom(this)
}

/** Pops the last dynamic state */
case class PopDynamicState() extends Step {
  lazy val inputValues = Set[TempValue](WorldPtrValue)
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    this
}

/** Performs an integer operation while checking for overflow */
sealed trait CheckedIntegerStep extends Step {
  val result: TempValue
  val val1: TempValue
  val val2: TempValue
  val overflowMessage: RuntimeErrorMessage

  lazy val inputValues = Set[TempValue](val1, val2, WorldPtrValue)
  lazy val outputValues = Set[TempValue](result)
}

/** Adds two integers of the same type */
case class CheckedIntegerAdd(
    result: TempValue,
    val1: TempValue,
    val2: TempValue,
    overflowMessage: RuntimeErrorMessage
) extends CheckedIntegerStep with DiscardableStep with MergeableStep {
  def renamed(f: (TempValue) => TempValue) =
    CheckedIntegerAdd(f(result), f(val1), f(val2), overflowMessage).assignLocationFrom(this)
}

/** Subtracts two integers of the same type */
case class CheckedIntegerSub(
    result: TempValue,
    val1: TempValue,
    val2: TempValue,
    overflowMessage: RuntimeErrorMessage
) extends CheckedIntegerStep with DiscardableStep with MergeableStep {
  def renamed(f: (TempValue) => TempValue) =
    CheckedIntegerSub(f(result), f(val1), f(val2), overflowMessage).assignLocationFrom(this)
}

/** Multiplies two integers of the same type */
case class CheckedIntegerMul(
    result: TempValue,
    val1: TempValue,
    val2: TempValue,
    overflowMessage: RuntimeErrorMessage
) extends CheckedIntegerStep with DiscardableStep with MergeableStep {
  def renamed(f: (TempValue) => TempValue) =
    CheckedIntegerMul(f(result), f(val1), f(val2), overflowMessage).assignLocationFrom(this)
}

/** Performs truncating division on two integers of the same type */
case class IntegerDiv(
    result: TempValue,
    signed: Boolean,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    IntegerDiv(f(result), signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Calculates the remainder of truncating division on two integers of the same type */
case class IntegerRem(
    result: TempValue,
    signed: Boolean,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    IntegerRem(f(result), signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Adds two floats of the same type */
case class FloatAdd(
    result: TempValue,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatAdd(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Subtracts two floats of the same type */
case class FloatSub(
    result: TempValue,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatSub(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Multiplies two floats of the same type */
case class FloatMul(
    result: TempValue,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatMul(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Divides two floats of the same type */
case class FloatDiv(
    result: TempValue,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatDiv(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

sealed abstract class CompareCond
object CompareCond {
  case object Equal extends CompareCond
  case object NotEqual extends CompareCond
  case object GreaterThan extends CompareCond
  case object GreaterThanEqual extends CompareCond
  case object LessThan extends CompareCond
  case object LessThanEqual extends CompareCond
}

/** Compares two integers and stores a predicate with the result
  *
  * This can also be used to compare two pointers of the same type
  */
case class IntegerCompare(
    result: TempValue,
    cond: CompareCond,
    signed: Option[Boolean],
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    IntegerCompare(f(result), cond, signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Performs an ordered comparison between two floating point values */
case class FloatCompare(
    result: TempValue,
    cond: CompareCond,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatCompare(f(result), cond, f(val1), f(val2)).assignLocationFrom(this)
}

/** Tests if a floating point value is NaN */
case class FloatIsNaN(
    result: TempValue,
    value: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](value)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatIsNaN(f(result), f(value)).assignLocationFrom(this)
}

/** Tests if two floating point values have exactly the same IEEE bitwise representation
  *
  * This is useful for distinguishing positive and negative zero from each other. NaNs or denormalised values cannot
  * be tested using this method as they have multiple equivalent encodings
  */
case class FloatBitwiseCompare(
    result: TempValue,
    val1: TempValue,
    val2: TempValue
) extends DiscardableStep with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f: (TempValue) => TempValue) =
    FloatBitwiseCompare(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

case class AssertPredicate(
    predicate: TempValue,
    errorMessage: RuntimeErrorMessage,
    evidenceOpt: Option[TempValue] = None
) extends Step with AssertStep {
  lazy val inputValues = Set(WorldPtrValue, predicate) ++ evidenceOpt.toSet
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    AssertPredicate(
      f(predicate),
      errorMessage,
      evidenceOpt.map(f)
    ).assignLocationFrom(this)

  override def mergeKey = (predicate)
}
