package io.llambda.compiler.planner.step
import io.llambda

import llambda.compiler.{ProcedureSignature, RuntimeErrorMessage}
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
    Temp(vt.IntrinsicCellType(cellType), knownConstant)
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

object EntryPointTemp {
  def apply() =
    new TempValue(false)
}

sealed trait Step {
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

/** Step requiring a cell from a temporary allocation */
sealed trait CellConsumer extends Step {
  val allocSize : Int
}

/** Step producing a value that can be merged with identical instances of itself
  *
  * These must satisfy the following properties:
  * - The step must not depend on global state
  * - The step must not have any side effects on global state besides potentially raising an error
  * - The source and destination values must either both be immutable or reference the same location in memory
  *
  * These are merged by conniver.MergeIdenticalSteps
  */
sealed trait MergeableStep extends Step {
  val result : TempValue
  
  def renamed(f : (TempValue) => TempValue) : MergeableStep
}

/** Argument passed to invoke
  *
  * @param  tempValue  Value to pass as the argument
  * @param  dispose    If true the value is disposed after being passed to the procedure. This effectively transfers
  *                    ownership of the value to the procedure and avoids the overhead of GC rooting the value by the
  *                    caller.
  */
case class InvokeArgument(
  tempValue : TempValue,
  dispose : Boolean = false
) {
  def renamed(f : (TempValue) => TempValue) = 
    InvokeArgument(f(tempValue), dispose)
}

/** Invokes an entry point with the given arguments
  *
  * Entry points can be loaded with StoreNamedEntryPoint
  */
case class Invoke(result : Option[TempValue], signature : ProcedureSignature, entryPoint : TempValue, arguments : List[InvokeArgument]) extends Step {
  lazy val inputValues = arguments.map(_.tempValue).toSet + entryPoint
  lazy val outputValues = result.toSet

  // The world arg is required for allocations
  override def canAllocate = signature.hasWorldArg
  
  def renamed(f : (TempValue) => TempValue) = 
    Invoke(
      result=result.map(f),
      signature=signature,
      entryPoint=f(entryPoint),
      arguments=arguments.map(_.renamed(f))
    )
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
  * Referencing a TempValue after DisposeValue has been called will fail at compile time. Disposing a GC managed value
  * will allow it to be garbage collected at the next allocaion if there are no other references to it
  */
case class DisposeValue(value : TempValue) extends Step {
  lazy val inputValues = Set[TempValue](value)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    DisposeValue(f(value))
}

/** Conditionally branches based on a value 
  *
  * @param result      location to store trueValue or falseValue when the branch
  *                    completes
  * @param test        i1 value to conditionally branch on
  * @param trueSteps   steps to perform if the condition is true
  * @param trueValue   value to place in result after performing trueSteps
  * @param falseSteps  steps to perform if the condition is false
  * @param falseValue  value to place in result after performing falseSteps
  */
case class CondBranch(result : TempValue, test : TempValue, trueSteps : List[Step], trueValue : TempValue, falseSteps : List[Step], falseValue : TempValue) extends Step {
  lazy val outerInputValues = Set(test)
  lazy val innerBranches = List((trueSteps, trueValue), (falseSteps, falseValue))
  
  lazy val outputValues = Set(result)
  lazy val inputValues =
    outerInputValues ++ 
    innerBranches.flatMap(_._1).flatMap(_.inputValues) ++
    innerBranches.map(_._2)

  def mapInnerBranches(mapper : (List[Step], TempValue) => (List[Step], TempValue)) = {
    val (mappedTrueSteps, mappedTrueValue) = mapper(trueSteps, trueValue)
    val (mappedFalseSteps, mappedFalseValue) = mapper(falseSteps, falseValue)

    CondBranch(result, test, mappedTrueSteps, mappedTrueValue, mappedFalseSteps, mappedFalseValue)
  }
  
  def renamed(f : (TempValue) => TempValue) =
    CondBranch(
      result=f(result),
      test=f(test),
      trueSteps=trueSteps.map(_.renamed(f)),
      trueValue=f(trueValue),
      falseSteps=falseSteps.map(_.renamed(f)),
      falseValue=f(falseValue)
    )
}

/** Tests if a cell is of a given type */
case class TestCellType(result : TempValue, value : TempValue, testType : ct.ConcreteCellType) extends Step with MergeableStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    TestCellType(f(result), f(value), testType)
}

/** Casts a cell to a subtype aborting if the cast is impossible */
case class CastCellToSubtypeChecked(result : TempValue, worldPtr : WorldPtrValue, value : TempValue, toType : ct.CellType, errorMessage : RuntimeErrorMessage) extends Step with MergeableStep {
  lazy val inputValues = Set(worldPtr, value)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    CastCellToSubtypeChecked(f(result), worldPtr, f(value), toType, errorMessage)
}

/** Casts a cell to another type without checking the validity of the cast */
case class CastCellToTypeUnchecked(result : TempValue, value : TempValue, toType : ct.CellType) extends Step with MergeableStep {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    CastCellToTypeUnchecked(f(result), f(value), toType)
}

/** Converts an native integer to another width and/or signedness */
case class ConvertNativeInteger(result : TempValue, fromValue : TempValue, toBits : Int, signed : Boolean) extends Step with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeInteger(f(result), f(fromValue), toBits, signed)
}

/** Converts an native float to another type */
case class ConvertNativeFloat(result : TempValue, fromValue : TempValue, toType : vt.FpType) extends Step with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeFloat(f(result), f(fromValue), toType)
}

/** Converts an native integer to a float */
case class ConvertNativeIntegerToFloat(result : TempValue, fromValue : TempValue, fromSigned: Boolean, toType : vt.FpType) extends Step with MergeableStep {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    ConvertNativeIntegerToFloat(f(result), f(fromValue), fromSigned, toType)
}
      
/** Builds a proper list at runtime
  *
  * @param result      location to store the head of the proper list
  * @param listValues  DatumCell values to add to the list
  */
case class BuildProperList(result : TempValue, listValues : List[TempValue]) extends Step with CellConsumer {
  lazy val inputValues = listValues.toSet
  lazy val outputValues = Set(result)

  val allocSize = listValues.length
  
  def renamed(f : (TempValue) => TempValue) =
    BuildProperList(f(result), listValues.map(f))
}

/** Calculates the length of a proper list as a uint32
  *
  * The passed list must be a proper list or the result is undefined
  */
case class CalcProperListLength(result : TempValue, listHead : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set(listHead)
  lazy val outputValues = Set(result)

  def renamed(f : (TempValue) => TempValue) =
    CalcProperListLength(f(result), f(listHead))
}

/** Indicates a step that stores a constant value */
sealed trait StoreConstant extends Step with MergeableStep {
  val result : TempValue
  lazy val outputValues = Set(result)
}

/** Stores an entry point with the given signature and native symbol
  *
  * This can be called with Invoke
  */
case class StoreNamedEntryPoint(result : TempValue, signature : ProcedureSignature, nativeSymbol : String) extends Step with MergeableStep {
  val inputValues = Set[TempValue]()
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StoreNamedEntryPoint(f(result), signature, nativeSymbol)
}

/** Indicates a step that stores a constant cell */
sealed trait StoreConstantCell extends StoreConstant 

case class StoreStringCell(result : TempValue, value : String) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreStringCell(f(result), value)
}

case class StoreSymbolCell(result : TempValue, value : String) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreSymbolCell(f(result), value)
}

case class StoreExactIntegerCell(result : TempValue, value : Long) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreExactIntegerCell(f(result), value)
}

case class StoreInexactRationalCell(result : TempValue, value : Double) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreInexactRationalCell(f(result), value)
}

case class StoreCharacterCell(result : TempValue, value : Char) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreCharacterCell(f(result), value)
}

case class StoreBooleanCell(result : TempValue, value : Boolean) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreBooleanCell(f(result), value)
}

case class StoreBytevectorCell(result : TempValue, elements : Vector[Short]) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreBytevectorCell(f(result), elements)
}

case class StorePairCell(result : TempValue, car : TempValue, cdr : TempValue, listLengthOpt : Option[Long], memberTypeOpt : Option[ct.ConcreteCellType]) extends StoreConstantCell {
  lazy val inputValues = Set(car, cdr)
  
  def renamed(f : (TempValue) => TempValue) =
    StorePairCell(f(result), f(car), f(cdr), listLengthOpt, memberTypeOpt)
}

case class StoreVectorCell(result : TempValue, elements : Vector[TempValue]) extends StoreConstantCell {
  lazy val inputValues = elements.toSet
  
  def renamed(f : (TempValue) => TempValue) =
    StoreVectorCell(f(result), elements.map(f))
}

case class StoreUnitCell(result : TempValue) extends StoreConstantCell {
  val inputValues = Set[TempValue]()  
  
  def renamed(f : (TempValue) => TempValue) =
    StoreUnitCell(f(result))
}

case class StoreEmptyListCell(result : TempValue) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    StoreEmptyListCell(f(result))
}

/** Stores a procedure with an empty closure 
  *
  * This is equalivent to RecordLikeInit(vt.EmptyClosureType] followed by StoreProcedureEntryPoint except it uses a
  * compile time constant cell and is considerably more efficient
  **/
case class StoreEmptyClosure(result : TempValue, entryPoint : TempValue) extends StoreConstantCell {
  lazy val inputValues = Set(entryPoint)
  
  def renamed(f : (TempValue) => TempValue) =
    StoreEmptyClosure(f(result), f(entryPoint))
}

/** Indicates a step that stores a native constant */
sealed trait StoreNativeConstant extends StoreConstant {
  val inputValues = Set[TempValue]()
}

case class StoreNativeInteger(result : TempValue, value : Long, bits : Int) extends StoreNativeConstant {
  def renamed(f : (TempValue) => TempValue) =
    StoreNativeInteger(f(result), value, bits)
}

case class StoreNativeFloat(result : TempValue, value : Double, fpType : vt.FpType) extends StoreNativeConstant {
  def renamed(f : (TempValue) => TempValue) =
    StoreNativeFloat(f(result), value, fpType)
}

/** Indicates a step that unboxes a cell */
sealed trait UnboxValue extends Step {
  val result : TempValue
  val boxed : TempValue

  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Stores if a cell is "truthy". All values except false are truthy. 
  *
  * This is mergeable because booleans are immutable and cells can't change types at runtime
  */
case class UnboxAsTruthy(result : TempValue, boxed : TempValue) extends UnboxValue with MergeableStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxAsTruthy(f(result), f(boxed))
}

case class UnboxExactInteger(result : TempValue, boxed : TempValue) extends UnboxValue with MergeableStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxExactInteger(f(result), f(boxed))
}

case class UnboxInexactRational(result : TempValue, boxed : TempValue) extends UnboxValue with MergeableStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxInexactRational(f(result), f(boxed))
}

case class UnboxCharacter(result : TempValue, boxed : TempValue) extends UnboxValue with MergeableStep {
  def renamed(f : (TempValue) => TempValue) =
    UnboxCharacter(f(result), f(boxed))
}

// These aren't quite an unboxing because there's two values per boxed value

/** Stores the car of the passed PairCell as a DatumCell */
case class StorePairCar(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StorePairCar(f(result), f(boxed))
}

/** Stores the cdr of the passed PairCell as a DatumCell */
case class StorePairCdr(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StorePairCdr(f(result), f(boxed))
}

/** Store the entry point of a procedure
  *
  * This is not mergeable to allow procedures to dynamically change entry points
  */
case class StoreProcedureEntryPoint(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StoreProcedureEntryPoint(f(result), f(boxed))
}

/** Stores the length of a vector as an Int32 */
case class StoreVectorLength(result : TempValue, boxed : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StoreVectorLength(f(result), f(boxed))
}

/** Indicates a step that boxes a native value
  *
  * These are mergeable because SSA guarantees native values can't change at runtime
  */
sealed trait BoxValue extends Step with MergeableStep {
  val result : TempValue
  val unboxed : TempValue
  
  lazy val outputValues = Set(result)
}

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result : TempValue, unboxed : TempValue) extends BoxValue {
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxBoolean(f(result), f(unboxed))
}

case class BoxExactInteger(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1

  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxExactInteger(f(result), f(unboxed))
}

case class BoxInexactRational(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1
  
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxInexactRational(f(result), f(unboxed))
}

case class BoxCharacter(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1
  
  lazy val inputValues = Set(unboxed)
  
  def renamed(f : (TempValue) => TempValue) =
    BoxCharacter(f(result), f(unboxed))
}

/** Returns from the current function */
case class Return(returnValue : Option[TempValue]) extends Step {
  lazy val inputValues = returnValue.toSet
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    Return(returnValue.map(f))
}

/** Allocates data for a given record a given type 
 *
 * @param cellResult  location to store the record cell 
 * @param dataResult  location to store the uninitialized record data 
 * @param recordType  type of record to create
 */
case class RecordLikeInit(cellResult : TempValue, dataResult : TempValue, recordLikeType : vt.RecordLikeType) extends Step with CellConsumer {
  val allocSize = 1

  val inputValues = Set[TempValue]()
  val outputValues = Set(cellResult, dataResult)
  
  def renamed(f : (TempValue) => TempValue) =
    RecordLikeInit(f(cellResult), f(dataResult), recordLikeType)
}

/** Sets a record field. The value must match the type of record field */
case class RecordDataFieldSet(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField, newValue : TempValue) extends Step {
  lazy val inputValues = Set(recordData, newValue)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    RecordDataFieldSet(f(recordData), recordLikeType, recordField, f(newValue))
}

/** Sets a record field as undefined
  *
  * If RecordDataFieldRef is later called it will raise a runtime error if checkUndef is true
  */
case class RecordDataFieldSetUndefined(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step {
  lazy val inputValues = Set(recordData)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    RecordDataFieldSetUndefined(f(recordData), recordLikeType, recordField)
}

/** Reads a record field. The value must match the type of record field */
case class RecordDataFieldRef(result : TempValue, recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step {
  lazy val inputValues = Set(recordData)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    RecordDataFieldRef(f(result), f(recordData), recordLikeType, recordField)
}

/** Asserts that field value is defined 
  *
  * The field must have previously been loaded with RecordDataFieldRef
  **/
case class AssertRecordDataFieldDefined(worldPtr : WorldPtrValue, fieldValue : TempValue, recordField : vt.RecordField, errorMessage : RuntimeErrorMessage) extends Step {
  lazy val inputValues = Set(worldPtr, fieldValue)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    AssertRecordDataFieldDefined(worldPtr, f(fieldValue), recordField, errorMessage)
}

/** Tests to see if a record is of a given class */
case class TestRecordLikeClass(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step with MergeableStep {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    TestRecordLikeClass(result, f(recordCell), recordLikeType)
}

/** Asserts that a record is of a given class 
  *
  * A runtime error will occur if the record isn't of the passed class
  */
case class AssertRecordLikeClass(worldPtr : WorldPtrValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType, errorMessage : RuntimeErrorMessage) extends Step {
  lazy val inputValues = Set(worldPtr, recordCell)
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) =
    AssertRecordLikeClass(worldPtr, f(recordCell), recordLikeType, errorMessage) 
}

/** Stores the data of a record */
case class StoreRecordLikeData(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
  
  def renamed(f : (TempValue) => TempValue) =
    StoreRecordLikeData(f(result), f(recordCell), recordLikeType)
}

/** Sets the entry point of a procedure
  *
  * The procedure should be created using RecordLikeInit with a ClosureType
  */
case class SetProcedureEntryPoint(procedureCell : TempValue, entryPoint : TempValue) extends Step {
  lazy val inputValues = Set(procedureCell, entryPoint)
  val outputValues = Set[TempValue]()
  
  def renamed(f : (TempValue) => TempValue) =
    SetProcedureEntryPoint(f(procedureCell), f(entryPoint))
}

/** Pushes a new dynamic state with the given parameter values
  * 
  * @param parameterValues  Map of parameter procedure IR values to the new value the paramer should take
  */
case class PushDynamicState(worldPtr : WorldPtrValue, parameterValues : List[(TempValue, TempValue)]) extends Step {
  lazy val inputValues = (parameterValues.flatMap { case (parameter, value) =>
    List(parameter, value)
  }).toSet + worldPtr
  val outputValues = Set[TempValue]()

  def renamed(f : (TempValue) => TempValue) = 
    PushDynamicState(worldPtr, parameterValues.map { case (parameter, value) =>
      f(parameter) -> f(value)
    })
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
case class IntegerAdd(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerAdd(f(result), f(val1), f(val2))
}

/** Subtracts two integers of the same type */
case class IntegerSub(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerSub(f(result), f(val1), f(val2))
}

/** Multiplies two integers of the same type */
case class IntegerMul(result : TempValue, val1 : TempValue, val2 : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerMul(f(result), f(val1), f(val2))
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
case class IntegerCompare(result : TempValue, cond : CompareCond.CompareCond, signed : Option[Boolean], val1 : TempValue, val2 : TempValue) extends Step with MergeableStep {
  lazy val inputValues = Set[TempValue](val1, val2)
  lazy val outputValues = Set[TempValue](result)
  
  def renamed(f : (TempValue) => TempValue) = 
    IntegerCompare(f(result), cond, signed, f(val1), f(val2))
}
