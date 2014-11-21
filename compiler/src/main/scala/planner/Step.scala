package io.llambda.compiler.planner.step
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated, RuntimeErrorMessage}
import llambda.compiler.ast
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

class TempValue(val isGcManaged : Boolean) {
  override def toString = s"%${this.hashCode.toHexString}" 
}

class WorldPtrValue extends TempValue(false)

object Temp {
  def apply(valueType : vt.ValueType, knownConstant : Boolean = false) =
    new TempValue(!knownConstant && valueType.isGcManaged)
}

object CellTemp {
  def apply(cellType : ct.CellType, knownConstant : Boolean = false) =
    // Open code this so we don't need to create a temporary vt.SchemeType which can be expensive
    new TempValue(!knownConstant && !cellType.isInstanceOf[ct.PreconstructedCellType])
}

object RecordTemp {
  def apply() =
    // Records are always GC managed
    new TempValue(true)
}

object ClosureTemp {
  def apply() =
    // Closures are always GC managed
    new TempValue(true)
}

object RecordLikeDataTemp {
  def apply() =
    new TempValue(false)
}

object VectorElementsTemp {
  def apply() =
    new TempValue(false)
}

object EntryPointTemp {
  def apply() =
    new TempValue(false)
}

sealed trait Step extends ContextLocated {
  val inputValues : Set[TempValue]
  val outputValues : Set[TempValue]

  /** Renames all the temp values this step references
    *
    * This is used to remove redundant values in the conniver
    */
  def renamed(f : (TempValue) => TempValue) : Step

  /** Indicates a step that can trigger a GC allocation
    *
    * This means the heap state has to be fully in sync - we can have no allocated but uninitialized conses, etc.
    */
  def canAllocate : Boolean = false
}

sealed trait NestingStep extends Step {
  /** Input values used by the nesting step itself
    *
    * This excludes any input values used by the inner branches
    */
  def outerInputValues : Set[TempValue]

  /** List of inner branches
    *
    * Branches are a tuple of (steps, outputValue)
    */
  def innerBranches : List[(List[Step], TempValue)]

  /** Rewrites this step with new inner branches */
  def mapInnerBranches(mapper : (List[Step], TempValue) => (List[Step], TempValue)) : NestingStep
  
  lazy val inputValues =
    outerInputValues ++ 
    innerBranches.flatMap(_._1).flatMap(_.inputValues) ++
    innerBranches.map(_._2)
}

/** Step requiring a cell from a temporary allocation */
sealed trait CellConsumer extends Step

/** Step that dispose its input values as part of the step 
  *
  * This is typically used for steps that are GC barriers so they can avoid rooting input values that will be unused
  * after the step completets
  */
sealed trait InputDisposableStep extends Step {
  def inputToDispose : Set[TempValue]
  def withDisposedInput(inputToDispose : Set[TempValue]) : InputDisposableStep
}

/** Step producing a value that can be disposed or merged with identical instances of itself
  *
  * These must satisfy the following properties:
  * - The step must not depend on global state
  * - The step must not have any side effects on global state besides potentially raising an error
  * - The source and destination values must either both be immutable or reference the same location in memory
  *
  * These are potentially disposed by ps.DisposeValues or merged by conniver.MergeIdenticalSteps
  */
sealed trait DisposableStep extends Step {
  val result : TempValue
  
  /** Key used to compare if two mergeable steps are the same
    * 
    * The default key is just the original mergeable step with the result renamed to a constant value. Other steps may
    * want to override this return a tuple of fields to exclude things like error messages.
    */
   def mergeKey : Any = {
     this.renamed({ tempValue =>
       if (tempValue == result) {
         DisposableStep.PlaceholderResultTemp
       }
       else {
         tempValue
       }
     })
   }
}

object DisposableStep {
  object PlaceholderResultTemp extends TempValue(false)
}

/** Step without any side effects
  *
  * Examples of nullipotent steps are loading values from memory, converting values or comparing values. These may be
  * disposed entirely by DisposeValues if its result is unused. Identical steps are also candidates for merging
  */
sealed trait NullipotentStep extends DisposableStep

/** Step with side effects
  *
  * Examples of idempotent steps are stores. Idempotent steps cannot be disposed even if their result is unused but they
  * can be merged with identical instances of themselves.
  */
sealed trait IdempotentStep extends DisposableStep

/** Step that can conditionally terminate execution based on a test */
sealed trait AssertStep extends IdempotentStep {
  val result = DisposableStep.PlaceholderResultTemp
}

sealed trait InvokeLike extends Step {
  val signature : ProcedureSignature

  // The world arg is required for allocations
  override def canAllocate = signature.hasWorldArg
}

/** Invokes an entry point with the given arguments
  *
  * Entry points can be loaded with CreateNamedEntryPoint
  */
case class Invoke(
    result : Option[TempValue],
    signature : ProcedureSignature,
    entryPoint : TempValue,
    arguments : List[TempValue],
    inputToDispose : Set[TempValue] = Set()
) extends InvokeLike with InputDisposableStep {
  lazy val inputValues = arguments.toSet + entryPoint
  lazy val outputValues = result.toSet
    
  def withDisposedInput(values : Set[TempValue]) =
    this.copy(inputToDispose=values).assignLocationFrom(this)

  def renamed(f : (TempValue) => TempValue) = 
    Invoke(
      result=result.map(f),
      signature=signature,
      entryPoint=f(entryPoint),
      arguments=arguments.map(f),
      inputToDispose=inputToDispose
    ).assignLocationFrom(this)
}

/** Invokes a procedure and immediately returns the value */
case class TailCall(signature : ProcedureSignature, entryPoint : TempValue, arguments : List[TempValue]) extends InvokeLike {
  lazy val inputValues = arguments.toSet + entryPoint
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) = 
    TailCall(
      signature=signature,
      entryPoint=f(entryPoint),
      arguments=arguments.map(f)
    ).assignLocationFrom(this)
}

/** Allocates a given number of cells at runtime 
 *
 * This should only be inserted in to the plan by PlanCellAllocations
 */
case class AllocateCells(worldPtr : WorldPtrValue, count : Int) extends Step {
  val inputValues = Set[TempValue](worldPtr)
  val outputValues = Set[TempValue]()

  override def canAllocate = true
  
  def renamed(f : (TempValue) => TempValue) = this
}

/** Permanently forgets about a temp value
  *
  * Referencing a TempValue after DisposeValues has been called will fail at compile time. Disposing a GC managed value
  * will allow it to be garbage collected at the next allocaion if there are no other references to it
  */
case class DisposeValues(values : Set[TempValue]) extends Step {
  lazy val inputValues = values
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    DisposeValues(values.map(f)).assignLocationFrom(this)
}

/** Conditionally branches based on a value 
  *
  * @param result      location to store trueValue or falseValue when the branch completes
  * @param test        i1 value to conditionally branch on
  * @param trueSteps   steps to perform if the condition is true
  * @param trueValue   value to place in result after performing trueSteps
  * @param falseSteps  steps to perform if the condition is false
  * @param falseValue  value to place in result after performing falseSteps
  */
case class CondBranch(result : TempValue, test : TempValue, trueSteps : List[Step], trueValue : TempValue, falseSteps : List[Step], falseValue : TempValue) extends NestingStep {
  lazy val outerInputValues = Set(test)
  lazy val innerBranches = List((trueSteps, trueValue), (falseSteps, falseValue))
  
  lazy val outputValues = Set(result)

  def mapInnerBranches(mapper : (List[Step], TempValue) => (List[Step], TempValue)) = {
    val (mappedTrueSteps, mappedTrueValue) = mapper(trueSteps, trueValue)
    val (mappedFalseSteps, mappedFalseValue) = mapper(falseSteps, falseValue)

    CondBranch(result, test, mappedTrueSteps, mappedTrueValue, mappedFalseSteps, mappedFalseValue)
      .assignLocationFrom(this)
  }
  
  def renamed(f : (TempValue) => TempValue) =
    CondBranch(
      result=f(result),
      test=f(test),
      trueSteps=trueSteps.map(_.renamed(f)),
      trueValue=f(trueValue),
      falseSteps=falseSteps.map(_.renamed(f)),
      falseValue=f(falseValue)
    ).assignLocationFrom(this)
}

/** Checks if the loop body returns true for every index */
case class ForAll(
    result : TempValue,
    loopCountValue : TempValue,
    loopIndexValue : TempValue,
    loopSteps : List[Step],
    loopResultPred : TempValue
) extends NestingStep {
  lazy val outerInputValues = Set(loopCountValue)
  lazy val innerBranches = List((loopSteps, loopResultPred))

  lazy val outputValues = Set(result)
  
  def mapInnerBranches(mapper : (List[Step], TempValue) => (List[Step], TempValue)) = {
    val (mappedLoopSteps, mappedLoopResultPred) = mapper(loopSteps, loopResultPred)

    ForAll(result, loopCountValue, loopIndexValue, mappedLoopSteps, mappedLoopResultPred)
      .assignLocationFrom(this)
  }
  
  def renamed(f : (TempValue) => TempValue) =
    ForAll(
      result=f(result),
      loopCountValue=f(loopCountValue),
      loopIndexValue=f(loopIndexValue),
      loopSteps=loopSteps.map(_.renamed(f)),
      loopResultPred=f(loopResultPred)
    ).assignLocationFrom(this)
}

/** Tests if a cell is of a given type */
case class TestCellType(
    result : TempValue,
    value : TempValue,
    testType : ct.ConcreteCellType,
    possibleTypes : Set[ct.ConcreteCellType] = ct.AnyCell.concreteTypes
) extends Step with NullipotentStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    TestCellType(f(result), f(value), testType, possibleTypes).assignLocationFrom(this)
  
  private case class MergeKey(value : TempValue, testType : ct.ConcreteCellType)

  override def mergeKey : Any =
    // Merge steps with different possibleTypes - it's just an optimisation hint
    MergeKey(value, testType)
}

/** Casts a cell to another type without checking the validity of the cast */
case class CastCellToTypeUnchecked(result : TempValue, value : TempValue, toType : ct.CellType) extends Step with NullipotentStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    CastCellToTypeUnchecked(f(result), f(value), toType).assignLocationFrom(this)
}

/** Converts an native integer to another width and/or signedness */
case class ConvertNativeInteger(result : TempValue, fromValue : TempValue, toBits : Int, signed : Boolean) extends Step with NullipotentStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeInteger(f(result), f(fromValue), toBits, signed).assignLocationFrom(this)
}

/** Converts an native float to another type */
case class ConvertNativeFloat(result : TempValue, fromValue : TempValue, toType : vt.FpType) extends Step with NullipotentStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeFloat(f(result), f(fromValue), toType).assignLocationFrom(this)
}

/** Converts an native integer to a float */
case class ConvertNativeIntegerToFloat(result : TempValue, fromValue : TempValue, fromSigned: Boolean, toType : vt.FpType) extends Step with NullipotentStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeIntegerToFloat(f(result), f(fromValue), fromSigned, toType).assignLocationFrom(this)
}
      
/** Calculates the length of a proper list as a uint32
  *
  * The passed list must be a proper list or the result is undefined
  */
case class CalcProperListLength(result : TempValue, listHead : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set(listHead)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    CalcProperListLength(f(result), f(listHead)).assignLocationFrom(this)
}

/** Indicates a step that creates a constant value */
sealed trait CreateConstant extends Step with NullipotentStep {
  val result : TempValue
  lazy val outputValues = Set(result)
}

/** Creates an entry point with the given signature and native symbol
  *
  * This can be called with Invoke
  */
case class CreateNamedEntryPoint(result : TempValue, signature : ProcedureSignature, nativeSymbol : String) extends Step with NullipotentStep {
  val inputValues = Set[TempValue]()
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    CreateNamedEntryPoint(f(result), signature, nativeSymbol).assignLocationFrom(this)
}

/** Indicates a step that creates a constant cell */
sealed trait CreateConstantCell extends CreateConstant 

case class CreateStringCell(result : TempValue, value : String) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateStringCell(f(result), value).assignLocationFrom(this)
}

case class CreateSymbolCell(result : TempValue, value : String) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateSymbolCell(f(result), value).assignLocationFrom(this)
}

case class CreateExactIntegerCell(result : TempValue, value : Long) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateExactIntegerCell(f(result), value).assignLocationFrom(this)
}

case class CreateFlonumCell(result : TempValue, value : Double) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateFlonumCell(f(result), value).assignLocationFrom(this)

  private case class MergeKey(value : Double)

  override def mergeKey : Any =
    // Use the long bits so +nan.0, -0.0 etc are handled correctly
    MergeKey(java.lang.Double.doubleToLongBits(value))
}

case class CreateCharCell(result : TempValue, value : Int) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateCharCell(f(result), value).assignLocationFrom(this)
}

case class CreateBooleanCell(result : TempValue, value : Boolean) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateBooleanCell(f(result), value).assignLocationFrom(this)
}

case class CreateBytevectorCell(result : TempValue, elements : Vector[Short]) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateBytevectorCell(f(result), elements).assignLocationFrom(this)
}

case class CreatePairCell(result : TempValue, car : TempValue, cdr : TempValue, listLengthOpt : Option[Long]) extends CreateConstantCell {
  lazy val inputValues = Set(car, cdr)
  
  def renamed(f : (TempValue) => TempValue) =
    CreatePairCell(f(result), f(car), f(cdr), listLengthOpt).assignLocationFrom(this)
}

case class CreateVectorCell(result : TempValue, elements : Vector[TempValue]) extends CreateConstantCell {
  lazy val inputValues = elements.toSet
  
  def renamed(f : (TempValue) => TempValue) =
    CreateVectorCell(f(result), elements.map(f)).assignLocationFrom(this)
}

case class CreateUnitCell(result : TempValue) extends CreateConstantCell {
  val inputValues = Set[TempValue]()  
  
  def renamed(f : (TempValue) => TempValue) =
    CreateUnitCell(f(result)).assignLocationFrom(this)
}

case class CreateEmptyListCell(result : TempValue) extends CreateConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    CreateEmptyListCell(f(result)).assignLocationFrom(this)
}

/** Creates a procedure with an empty closure 
  *
  * This is equalivent to RecordLikeInit(vt.EmptyClosureType] followed by LoadProcedureEntryPoint except it uses a
  * compile time constant cell and is considerably more efficient
  **/
case class CreateEmptyClosure(result : TempValue, entryPoint : TempValue) extends CreateConstantCell {
  lazy val inputValues = Set(entryPoint)
  
  def renamed(f : (TempValue) => TempValue) =
    CreateEmptyClosure(f(result), f(entryPoint)).assignLocationFrom(this)
}

/** Indicates a step that creates a native constant */
sealed trait CreateNativeConstant extends CreateConstant {
  val inputValues = Set[TempValue]()
}

case class CreateNativeInteger(result : TempValue, value : Long, bits : Int) extends CreateNativeConstant {
  def renamed(f : (TempValue) => TempValue) =
    CreateNativeInteger(f(result), value, bits).assignLocationFrom(this)
}

case class CreateNativeFloat(result : TempValue, value : Double, fpType : vt.FpType) extends CreateNativeConstant {
  def renamed(f : (TempValue) => TempValue) =
    CreateNativeFloat(f(result), value, fpType).assignLocationFrom(this)

  private case class MergeKey(value : Double, fpType : vt.FpType)

  override def mergeKey : Any =
    // Use the long bits so +nan.0, -0.0 etc are handled correctly
    MergeKey(java.lang.Double.doubleToLongBits(value), fpType)
}

/** Indicates a step that unboxes a cell */
sealed trait UnboxValue extends Step {
  val result : TempValue
  val boxed : TempValue

  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

case class UnboxExactInteger(result : TempValue, boxed : TempValue) extends UnboxValue with NullipotentStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxExactInteger(f(result), f(boxed)).assignLocationFrom(this)
}

case class UnboxFlonum(result : TempValue, boxed : TempValue) extends UnboxValue with NullipotentStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxFlonum(f(result), f(boxed)).assignLocationFrom(this)
}

case class UnboxChar(result : TempValue, boxed : TempValue) extends UnboxValue with NullipotentStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxChar(f(result), f(boxed)).assignLocationFrom(this)
}

// These aren't quite an unboxing because there's two values per boxed value

/** Loads the car of the passed PairCell as a AnyCell */
case class LoadPairCar(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadPairCar(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads the cdr of the passed PairCell as a AnyCell */
case class LoadPairCdr(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadPairCdr(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads the entry point of a procedure
  *
  * This is not mergeable to allow procedures to dynamically change entry points
  */
case class LoadProcedureEntryPoint(result : TempValue, boxed : TempValue, signature : ProcedureSignature) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadProcedureEntryPoint(f(result), f(boxed), signature)
      .assignLocationFrom(this)
}

/** Loads the length of a string in characters as a UInt32
  *
  * This is nullipotent as a string's character length is immutable
  */
case class LoadStringCharLength(result : TempValue, boxed : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    LoadStringCharLength(f(result), f(boxed))
      .assignLocationFrom(this)
}

/** Loads the length of a symbol in bytes as a UInt16
  *
  * This is nullipotent as a symbol's byte length is immutable
  */
case class LoadSymbolByteLength(
    result : TempValue,
    boxed : TempValue,
    possibleLengthsOpt : Option[Set[Int]] = None
) extends Step with NullipotentStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
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
    result : TempValue,
    boxed : TempValue,
    offset : TempValue,
    symbolByteLength : Long,
    possibleValuesOpt : Option[Set[Byte]] = None
) extends Step with NullipotentStep {
  lazy val inputValues = Set(boxed, offset)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    LoadSymbolByte(f(result), f(boxed), f(offset), symbolByteLength, possibleValuesOpt)
      .assignLocationFrom(this)
}

/** Loads the length of a bytevector as a UInt32 */
case class LoadBytevectorLength(result : TempValue, boxed : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    LoadBytevectorLength(f(result), f(boxed)).assignLocationFrom(this)
}

/** Creates a new uninitialised vector of the given length
  *
  * All elements need to be initialised before it is accessed or the next GC barrier
  *
  * @param  vectorResult    Boxed vector cell result
  * @param  elementsResult  Vector element data result
  * @param  length          Number of elements in the newly allocated vector
  */
case class InitVector(
    vectorResult : TempValue,
    elementsResult : TempValue,
    length : TempValue
) extends Step with CellConsumer {
  lazy val inputValues = Set(length)
  lazy val outputValues = Set(vectorResult, elementsResult)

  def renamed(f : (TempValue) => TempValue) =
    InitVector(f(vectorResult), f(elementsResult), f(length)).assignLocationFrom(this)
}

/** Loads the pointer to the vector element data */
case class LoadVectorElementsData(result : TempValue, vectorCell : TempValue) extends Step with NullipotentStep { 
  lazy val inputValues = Set(vectorCell)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadVectorElementsData(f(result), f(vectorCell)).assignLocationFrom(this)
}

/** Loads the length of a vector as an Int32 */
case class LoadVectorLength(result : TempValue, boxed : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadVectorLength(f(result), f(boxed)).assignLocationFrom(this)
}

/** Loads an element from a vector 
  *
  * @param  vectorCell  Vector to load an element from
  * @param  elements    Vector elements pointer
  * @param  index       Index of the element to load as a UInt32. This value must be determined to be in range
  */
case class LoadVectorElement(
    result : TempValue,
    vectorCell : TempValue,
    elements : TempValue,
    index : TempValue
) extends Step {
  lazy val inputValues = Set(vectorCell, elements, index)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    LoadVectorElement(f(result), f(vectorCell), f(elements), f(index)).assignLocationFrom(this) 
}

/** Store an element in a vector 
  *
  * @param  vectorCell  Vector to load an element from
  * @param  elements    Vector elements pointer
  * @param  index       Index of the element to load as a UInt32. This value must be determined to be in range
  * @param  newValue    Boxed value to store at the element index 
  */
case class StoreVectorElement(
    vectorCell : TempValue,
    elements : TempValue,
    index : TempValue,
    newValue : TempValue
) extends Step {
  lazy val inputValues = Set(vectorCell, elements, index, newValue)
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) =
    StoreVectorElement(f(vectorCell), f(elements), f(index), f(newValue)).assignLocationFrom(this) 
}

/** Indicates a step that boxes a native value
  *
  * These are mergeable because SSA guarantees native values can't change at runtime
  */
sealed trait BoxValue extends Step with NullipotentStep {
  val result : TempValue
  val unboxed : TempValue
  
  lazy val outputValues = Set(result)
}

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result : TempValue, unboxed : TempValue) extends BoxValue {
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxBoolean(f(result), f(unboxed)).assignLocationFrom(this)
}

case class BoxExactInteger(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxExactInteger(f(result), f(unboxed)).assignLocationFrom(this)
}

case class BoxFlonum(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxFlonum(f(result), f(unboxed)).assignLocationFrom(this)
}

case class BoxChar(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxChar(f(result), f(unboxed)).assignLocationFrom(this)
}

/** Returns from the current function */
case class Return(returnValue : Option[TempValue]) extends Step {
  lazy val inputValues = returnValue.toSet
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    Return(returnValue.map(f)).assignLocationFrom(this)
}

/** Initialises a new pair with an undefined car and cdr
  *
  * SetPairCar and SetPairCdr must be called on the new pair before it is accessed or the next GC barrier
  */
case class InitPair(result : TempValue, listLengthOpt : Option[Int] = None) extends Step with CellConsumer {
  val inputValues = Set[TempValue]()
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    InitPair(f(result), listLengthOpt).assignLocationFrom(this)
}

/** Asserts that a pair is mutable
  *
  * It is illegal to attempt SetPairCar or SetPairCdr on an immutable pair
  */
case class AssertPairMutable(worldPtr : WorldPtrValue, pairValue : TempValue, errorMessage : RuntimeErrorMessage) extends AssertStep {
  lazy val inputValues = Set[TempValue](worldPtr, pairValue)
  val outputValues = Set[TempValue]()

  def renamed(f: (TempValue) => TempValue) =
    AssertPairMutable(worldPtr, f(pairValue), errorMessage).assignLocationFrom(this)

  override def mergeKey = 
    (worldPtr, pairValue)
}

case class SetPairCar(pairValue : TempValue, newValue : TempValue) extends Step {
  lazy val inputValues = Set[TempValue](pairValue, newValue)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetPairCar(f(pairValue), f(newValue)).assignLocationFrom(this)
}

case class SetPairCdr(pairValue : TempValue, newValue : TempValue) extends Step {
  lazy val inputValues = Set[TempValue](pairValue, newValue)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetPairCdr(f(pairValue), f(newValue)).assignLocationFrom(this)
}

/** Allocates a cell for a record of a given type 
 *
 * @param cellResult   location to store the record cell 
 * @param dataResult   location to store the uninitialized record data 
 * @param recordType   type of record to create
 * @param isUndefined  flag indicating if this value should be initially marked undefined. This can be tested with 
 *                     AssertRecordLikeDefined. This is used in the implementation of recursive values
 */
case class InitRecordLike(cellResult : TempValue, dataResult : TempValue, recordLikeType : vt.RecordLikeType, isUndefined : Boolean) extends Step with CellConsumer {
  val inputValues = Set[TempValue]()
  val outputValues = Set(cellResult, dataResult)
  
  def renamed(f : (TempValue) => TempValue) =
    InitRecordLike(f(cellResult), f(dataResult), recordLikeType, isUndefined).assignLocationFrom(this)
}

/** Sets a record as defined */
case class SetRecordLikeDefined(record : TempValue, recordLikeType : vt.RecordLikeType) extends Step {
  lazy val inputValues = Set(record)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetRecordLikeDefined(f(record), recordLikeType).assignLocationFrom(this)
}

/** Asserts that a record is defined */
case class AssertRecordLikeDefined(worldPtr : WorldPtrValue, record : TempValue, recordLikeType : vt.RecordLikeType, errorMessage : RuntimeErrorMessage) extends Step with AssertStep {
  lazy val inputValues = Set(worldPtr, record)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    AssertRecordLikeDefined(worldPtr, f(record), recordLikeType, errorMessage).assignLocationFrom(this)
}

/** Sets a record field. The value must match the type of record field */
case class SetRecordDataField(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField, newValue : TempValue) extends Step {
  lazy val inputValues = Set(recordData, newValue)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetRecordDataField(f(recordData), recordLikeType, recordField, f(newValue)).assignLocationFrom(this)
}

/** Reads a record field. The value must match the type of record field */
case class LoadRecordDataField(result : TempValue, recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step {
  lazy val inputValues = Set(recordData)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadRecordDataField(f(result), f(recordData), recordLikeType, recordField).assignLocationFrom(this)
}

/** Tests to see if a record is of a given class */
case class TestRecordLikeClass(
    result : TempValue,
    recordCell : TempValue,
    recordLikeType : vt.RecordLikeType,
    possibleTypesOpt : Option[Set[vt.RecordLikeType]] = None
) extends Step with NullipotentStep {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    TestRecordLikeClass(result, f(recordCell), recordLikeType, possibleTypesOpt).assignLocationFrom(this)
}

/** Loads the data of a record 
  *
  * Note this cannot be safely merged as this may point to inside a movable GC managed value
  */
case class LoadRecordLikeData(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadRecordLikeData(f(result), f(recordCell), recordLikeType).assignLocationFrom(this)
}

/** Sets the entry point of a procedure
  *
  * The procedure should be created using RecordLikeInit with a ClosureType
  */
case class SetProcedureEntryPoint(procedureCell : TempValue, entryPoint : TempValue) extends Step {
  lazy val inputValues = Set(procedureCell, entryPoint)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetProcedureEntryPoint(f(procedureCell), f(entryPoint)).assignLocationFrom(this)
}

/** Creates a new parameter procedure
  *
  * @param  result            Location to store the newly allocated ProcedureCell in
  * @param  initialValue      Initial value of the parameter procedure. This should be unconverted by converterProc
  * @param  converterProcOpt  Optional converter procedure to be applied to the initialValue and any future calls to
  *                           parameterize
  */
case class CreateParameterProc(
    worldPtr : WorldPtrValue,
    result : TempValue,
    initialValue : TempValue,
    converterProcOpt : Option[TempValue],
    inputToDispose : Set[TempValue] = Set()
) extends Step with InputDisposableStep {
  lazy val inputValues = Set(worldPtr, initialValue) ++ converterProcOpt.toSet
  lazy val outputValues = Set(result)
  
  override def canAllocate = true

  def withDisposedInput(values : Set[TempValue]) =
    this.copy(inputToDispose=values).assignLocationFrom(this)

  def renamed(f : (TempValue) => TempValue) =
    CreateParameterProc(
      worldPtr,
      f(result),
      f(initialValue),
      converterProcOpt.map(f),
      inputToDispose
    ).assignLocationFrom(this)
}

/** Loads the current value of a parameter procedure
  *
  * This equivalent to applying the parameter procedure but can be implemented more efficiently. In particular, this
  * doesn't require a GC barrier. 
  *
  * @param  result         Location to store the AnyCell value for the parameter procedure
  * @param  parameterProc  Parameter procdure to load the value for. The result of passing another procedure here is
  *                        undefined.
  */
case class LoadValueForParameterProc(
    worldPtr : WorldPtrValue,
    result : TempValue,
    parameterProc : TempValue
) extends Step {
  lazy val inputValues = Set(worldPtr, parameterProc)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    LoadValueForParameterProc(worldPtr, f(result), f(parameterProc)).assignLocationFrom(this)
}

/** Represents a value to be parameterized by PushDynamicSate
  *
  * @param  parameterProc         Parameter procedure to set the value for. A runtime error will be signalled if this is
  *                               another type of procedure
  * @param  newValue              New value for the parameter procedure. This will be automatically converted if the
  *                               was created with a converte procedure
  * @param  mayHaveConverterProc  Indicates if the set parameter may have a converter procedure. IF this is false more
  *                               efficient code can be generated by avoiding additional GC barriers. If this can't be
  *                               statically determined it should be set to true.
  */ 
case class ParameterizedValue(
    parameterProc : TempValue,
    newValue : TempValue,
    mayHaveConverterProc : Boolean = true
) {
  def renamed(f : TempValue => TempValue) =
    ParameterizedValue(f(parameterProc), f(newValue), mayHaveConverterProc)
}

/** Pushes a new dynamic state with the given parameter values
  * 
  * @param parameterValues  Map of parameter procedure IR values to the new value the paramer should take
  */
case class PushDynamicState(worldPtr : WorldPtrValue, parameterValues : List[ParameterizedValue]) extends Step {
  lazy val inputValues = parameterValues.flatMap({
    case ParameterizedValue(parameterProc, newValue, _) =>
      List(parameterProc, newValue)
  }).toSet + worldPtr

  val outputValues = Set[TempValue]()
  
  override def canAllocate : Boolean = true

  def renamed(f : (TempValue) => TempValue) = 
    PushDynamicState(worldPtr, parameterValues.map(_.renamed(f)))
      .assignLocationFrom(this)
}

/** Pops the last dynamic state 
  * 
  * This must be a state pushed with PushDynamicState, not through stdlib functions such as dynamic-wind
  */
case class PopDynamicState(worldPtr : WorldPtrValue) extends Step {
  lazy val inputValues = Set[TempValue](worldPtr)
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) = 
    this
}

/** Adds two integers of the same type */
case class IntegerAdd(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerAdd(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Subtracts two integers of the same type */
case class IntegerSub(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerSub(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Multiplies two integers of the same type */
case class IntegerMul(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerMul(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Performs truncating division on two integers of the same type */
case class IntegerDiv(result : TempValue, signed : Boolean, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f : (TempValue) => TempValue) =
    IntegerDiv(f(result), signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Calculats the remainder of truncating division on two integers of the same type */
case class IntegerRem(result : TempValue, signed : Boolean, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f : (TempValue) => TempValue) =
    IntegerRem(f(result), signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Adds two floats of the same type */
case class FloatAdd(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatAdd(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Subtracts two floats of the same type */
case class FloatSub(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatSub(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Multiplies two floats of the same type */
case class FloatMul(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatMul(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

/** Divides two floats of the same type */
case class FloatDiv(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatDiv(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

object CompareCond {
  sealed abstract class CompareCond

  case object Equal extends CompareCond
  case object NotEqual extends CompareCond
  case object GreaterThan extends CompareCond
  case object GreaterThanEqual extends CompareCond
  case object LessThan extends CompareCond
  case object LessThanEqual extends CompareCond
}

/** Compares two integers and stores a predicate with the result
  *
  * This can also be used to compare two pointers of the same type, GC managed or otherwise.
  */
case class IntegerCompare(result : TempValue, cond : CompareCond.CompareCond, signed : Option[Boolean], val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerCompare(f(result), cond, signed, f(val1), f(val2)).assignLocationFrom(this)
}

/** Performs an ordered comparison between two floating point values */
case class FloatCompare(result : TempValue, cond : CompareCond.CompareCond, val1 : TempValue, val2 : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatCompare(f(result), cond, f(val1), f(val2)).assignLocationFrom(this)
}

/** Tests if a floating point value is NaN */
case class FloatIsNaN(result : TempValue, value : TempValue) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](value)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    FloatIsNaN(f(result), f(value)).assignLocationFrom(this)
}

/** Tests if two floating point values have exactly the same IEEE bitwise representation
  *
  * This is useful for distinguishing positive and negative zero from each other. NaNs or denormalised values cannot
  * be tested using this method as they have multiple equivalent encodings
  */
case class FloatBitwiseCompare(
    result : TempValue,
    val1 : TempValue,
    val2 : TempValue
) extends Step with NullipotentStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)

  def renamed(f : (TempValue) => TempValue) =
    FloatBitwiseCompare(f(result), f(val1), f(val2)).assignLocationFrom(this)
}

case class AssertPredicate(
    worldPtr : WorldPtrValue,
    predicate : TempValue,
    errorMessage : RuntimeErrorMessage,
    evidenceOpt : Option[TempValue] = None
) extends Step with AssertStep {
  lazy val inputValues = Set(worldPtr, predicate) ++ evidenceOpt.toSet
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) =
    AssertPredicate(
      worldPtr,
      f(predicate),
      errorMessage,
      evidenceOpt.map(f)
    ).assignLocationFrom(this)

  override def mergeKey = 
    (worldPtr, predicate)
}
