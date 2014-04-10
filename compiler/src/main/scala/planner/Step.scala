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

object GcManagedValue {
  /** Creates a new GC managed temp value */
  def apply() : TempValue = 
    new TempValue(true)
}

object GcUnmanagedValue {
  /** Creates a unmanaged temp value */
  def apply() : TempValue =
    new TempValue(false)
}

sealed trait Step {
  val inputValues : Set[TempValue]
  val outputValues : Set[TempValue]

  /** Indicates a step that can trigger a GC allocation
   *
   * This means the heap state has to be fully in sync - we can have no
   * allocated but uninitialized conses, etc.
   */
  def canAllocate : Boolean = false
}

/** Step requiring a cell from a temporary allocation */
sealed trait CellConsumer extends Step {
  val allocSize : Int
}

/** Step containing inner steps 
 *
 * These are typically used for steps that need to be aware of exceptions or
 * continuations or for conditionals
 */
sealed trait NestingStep extends Step {
  val outerInputValues : Set[TempValue]
  val innerBranches : List[(List[Step], TempValue)]
  
  lazy val inputValues =
    outerInputValues ++
    innerBranches.flatMap(_._1).flatMap(_.inputValues) ++
    innerBranches.map(_._2)

  def mapInnerBranches(mapper : (List[Step], TempValue) => List[Step]) : NestingStep 
}


/** Argument passed to invoke
  *
  * @param  tempValue  Value to pass as the argument
  * @param  dispose    If true the value is disposed after being passed to the
  *                    procedure. This effectively transfers ownership of the
  *                    value to the procedure and avoids the overhead of GC 
  *                    rooting the value by the caller.
  */
case class InvokeArgument(
  tempValue : TempValue,
  dispose : Boolean = false
)

/** Invokes an entry point with the given arguments
  *
  * Entry points can be loaded with StoreNamedEntryPoint
  */
case class Invoke(result : Option[TempValue], signature : ProcedureSignature, entryPoint : TempValue, arguments : List[InvokeArgument]) extends Step {
  lazy val inputValues = arguments.map(_.tempValue).toSet + entryPoint
  lazy val outputValues = result.toSet

  // The world arg is required for allocations
  override def canAllocate = signature.hasWorldArg
}

/** Allocates a given number of cells at runtime 
 *
 * This should only be inserted in to the plan by PlanCellAllocations
 */
case class AllocateCells(worldPtr : WorldPtrValue, count : Int) extends Step {
  val inputValues = Set[TempValue](worldPtr)
  val outputValues = Set[TempValue]()

  override def canAllocate = true
}

/** Permanently forgets about a temp value
  *
  * Referencing a TempValue after DisposeValue has been called will fail at
  * compile time. Disposing a GC managed value will allow it to be garbage
  * collected at the next allocaion if there are no other references to it
  */
case class DisposeValue(value : TempValue) extends Step {
  lazy val inputValues = Set[TempValue](value)
  val outputValues = Set[TempValue]()
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
case class CondBranch(result : TempValue, test : TempValue, trueSteps : List[Step], trueValue : TempValue, falseSteps : List[Step], falseValue : TempValue) extends NestingStep {
  lazy val outputValues = Set(result)

  lazy val outerInputValues = Set(test)
  lazy val innerBranches = List((trueSteps, trueValue), (falseSteps, falseValue))

  def mapInnerBranches(mapper : (List[Step], TempValue) => List[Step]) =
    CondBranch(result, test, mapper(trueSteps, trueValue), trueValue, mapper(falseSteps, falseValue), falseValue)
}

/** Tests if a cell is of a given type */
case class TestCellType(result : TempValue, value : TempValue, testType : ct.ConcreteCellType) extends Step {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
}

/** Casts a cell to a subtype aborting if the cast is impossible */
case class CastCellToSubtypeChecked(result : TempValue, worldPtr : WorldPtrValue, value : TempValue, toType : ct.CellType, errorMessage : RuntimeErrorMessage) extends Step {
  lazy val inputValues = Set(worldPtr, value)
  lazy val outputValues = Set(result)
}

/** Casts a cell to another type without checking the validity of the cast */
case class CastCellToTypeUnchecked(result : TempValue, value : TempValue, toType : ct.CellType) extends Step {
  lazy val inputValues = Set(value)
  lazy val outputValues = Set(result)
}

/** Converts an native integer to another width and/or signedness */
case class ConvertNativeInteger(result : TempValue, fromValue : TempValue, toBits : Int, signed : Boolean) extends Step {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
}

/** Converts an native float to another type */
case class ConvertNativeFloat(result : TempValue, fromValue : TempValue, toType : vt.FpType) extends Step {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
}

/** Converts an native integer to a float */
case class ConvertNativeIntegerToFloat(result : TempValue, fromValue : TempValue, fromSigned: Boolean, toType : vt.FpType) extends Step {
  lazy val inputValues = Set(fromValue)
  lazy val outputValues = Set(result)
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
}

/** Indicates a step that stores a constant value */
sealed trait StoreConstant extends Step {
  val result : TempValue
  lazy val outputValues = Set(result)
}

/** Stores an entry point with the given signature and native symbol
  *
  * This can be called with Invoke
  */
case class StoreNamedEntryPoint(result : TempValue, signature : ProcedureSignature, nativeSymbol : String) extends Step {
  val inputValues = Set[TempValue]()
  lazy val outputValues = Set(result)
}

/** Indicates a step that stores a constant cell */
sealed trait StoreConstantCell extends StoreConstant 

case class StoreStringCell(result : TempValue, value : String) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreSymbolCell(result : TempValue, value : String) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreExactIntegerCell(result : TempValue, value : Long) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreInexactRationalCell(result : TempValue, value : Double) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreCharacterCell(result : TempValue, value : Char) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreBooleanCell(result : TempValue, value : Boolean) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StoreBytevectorCell(result : TempValue, elements : Vector[Short]) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

case class StorePairCell(result : TempValue, car : TempValue, cdr : TempValue) extends StoreConstantCell {
  lazy val inputValues = Set(car, cdr)
}

case class StoreVectorCell(result : TempValue, elements : Vector[TempValue]) extends StoreConstantCell {
  lazy val inputValues = elements.toSet
}

case class StoreUnitCell(result : TempValue) extends StoreConstantCell {
  val inputValues = Set[TempValue]()  
}

case class StoreEmptyListCell(result : TempValue) extends StoreConstantCell {
  val inputValues = Set[TempValue]()
}

/** Stores a procedure with an empty closure 
  *
  * This is equalivent to RecordLikeInit(vt.EmptyClosureType] followed by
  * StoreProcedureEntryPoint except it uses a compile time constani cell and 
  * is considerably more efficient
  **/
case class StoreEmptyClosure(result : TempValue, entryPoint : TempValue) extends StoreConstantCell {
  lazy val inputValues = Set(entryPoint)
}

/** Indicates a step that stores a native constant */
sealed trait StoreNativeConstant extends StoreConstant {
  val inputValues = Set[TempValue]()
}

case class StoreNativeInteger(result : TempValue, value : Long, bits : Int) extends StoreNativeConstant
case class StoreNativeFloat(result : TempValue, value : Double, fpType : vt.FpType) extends StoreNativeConstant

/** Indicates a step that unboxes a cell */
sealed trait UnboxValue extends Step {
  val result : TempValue
  val boxed : TempValue

  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Stores if a cell is "truthy". All values except false are truthy. */
case class UnboxAsTruthy(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxExactInteger(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxInexactRational(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxCharacter(result : TempValue, boxed : TempValue) extends UnboxValue

// These aren't quite an unboxing because there's two values per boxed value

/** Stores the car of the passed PairCell as a DatumCell */
case class StorePairCar(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Stores the cdr of the passed PairCell as a DatumCell */
case class StorePairCdr(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Store the entry point of a procedure */
case class StoreProcedureEntryPoint(result : TempValue, boxed : TempValue) extends Step {
  lazy val inputValues = Set(boxed)
  lazy val outputValues = Set(result)
}

/** Indicates a step that boxes a native value */
sealed trait BoxValue extends Step {
  val result : TempValue
  val unboxed : TempValue
  
  lazy val outputValues = Set(result)
}

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result : TempValue, unboxed : TempValue) extends BoxValue {
  lazy val inputValues = Set(unboxed)
}

case class BoxExactInteger(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1

  lazy val inputValues = Set(unboxed)
}

case class BoxInexactRational(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1
  
  lazy val inputValues = Set(unboxed)
}

case class BoxCharacter(result : TempValue, unboxed : TempValue) extends BoxValue with CellConsumer {
  val allocSize = 1
  
  lazy val inputValues = Set(unboxed)
}

/** Returns from the current function */
case class Return(returnValue : Option[TempValue]) extends Step {
  lazy val inputValues = returnValue.toSet
  val outputValues = Set[TempValue]()
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
}

/** Sets a record field. The value must match the type of record field */
case class RecordDataFieldSet(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField, newValue : TempValue) extends Step {
  lazy val inputValues = Set(recordData, newValue)
  val outputValues = Set[TempValue]()
}

/** Sets a record field as undefined
  *
  * If RecordDataFieldRef is later called it will raise a runtime error if checkUndef is true
  */
case class RecordDataFieldSetUndefined(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step {
  lazy val inputValues = Set(recordData)
  val outputValues = Set[TempValue]()
}

/** Reads a record field. The value must match the type of record field */
case class RecordDataFieldRef(result : TempValue, recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step {
  lazy val inputValues = Set(recordData)
  lazy val outputValues = Set(result)
}

/** Asserts that field value is defined 
  *
  * The field must have previously been loaded with RecordDataFieldRef
  **/
case class AssertRecordDataFieldDefined(worldPtr : WorldPtrValue, fieldValue : TempValue, recordField : vt.RecordField, errorMessage : RuntimeErrorMessage) extends Step {
  lazy val inputValues = Set(worldPtr, fieldValue)
  val outputValues = Set[TempValue]()
}

/** Tests to see if a record is of a given class */
case class TestRecordLikeClass(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
}

/** Asserts that a record is of a given class 
  *
  * A runtime error will occur if the record isn't of the passed class
  */
case class AssertRecordLikeClass(worldPtr : WorldPtrValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType, errorMessage : RuntimeErrorMessage) extends Step {
  lazy val inputValues = Set(worldPtr, recordCell)
  val outputValues = Set[TempValue]()
}

/** Stores the data of a record */
case class StoreRecordLikeData(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step {
  lazy val inputValues = Set(recordCell)
  lazy val outputValues = Set(result)
}

/** Sets the entry point of a procedure
  *
  * The procedure should be created using RecordLikeInit with a ClosureType
  */
case class SetProcedureEntryPoint(procedureCell : TempValue, entryPoint : TempValue) extends Step {
  lazy val inputValues = Set(procedureCell, entryPoint)
  val outputValues = Set[TempValue]()
}

/** Executes the inner steps with a new dynamic environment containing the passed parameter values */
case class Parameterize(result : TempValue, worldPtr : WorldPtrValue, parameterValues : List[(TempValue, TempValue)], steps : List[Step], innerResult : TempValue) extends NestingStep {
  lazy val outputValues = Set(result)  
  
  lazy val outerInputValues = (parameterValues.flatMap { case (parameter, value) =>
    List(parameter, value)
  }).toSet + worldPtr

  lazy val innerBranches = List((steps, innerResult))

  def mapInnerBranches(mapper : (List[Step], TempValue) => List[Step]) =
    Parameterize(result, worldPtr, parameterValues, mapper(steps, innerResult), innerResult)
}
