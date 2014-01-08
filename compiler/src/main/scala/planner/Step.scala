package io.llambda.compiler.planner.step
import io.llambda

import llambda.compiler.{ProcedureSignature, RuntimeErrorMessage}
import llambda.compiler.ast
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

final class TempValue {
  override def toString = s"%${this.hashCode.toHexString}" 
}

final class TempAllocation {
  override def toString = s"%alloc-${this.hashCode.toHexString}" 
}

sealed trait Step 

/** Indicates a step that can trigger a GC allocation
  *
  * This means the heap state has to be fully in sync - we can have no
  * allocated but uninitialized conses, etc.
  */
sealed trait GcBarrier extends Step

/** Invokes an entry point with the given arguments
  *
  * Entry points can be loaded with StoreNamedEntryPoint */
case class Invoke(result : Option[TempValue], signature : ProcedureSignature, entryPoint : TempValue, arguments : List[TempValue]) extends Step with GcBarrier

/** Allocates a given number of cells at runtime */
case class AllocateCells(result : TempAllocation, count : Int) extends Step with GcBarrier

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
case class CondBranch(result : TempValue, test : TempValue, trueSteps : List[Step], trueValue : TempValue, falseSteps : List[Step], falseValue : TempValue) extends Step

/** Tests if a cell is of a given type */
case class TestCellType(result : TempValue, value : TempValue, testType : ct.ConcreteCellType) extends Step

/** Casts a cell to a subtype aborting if the cast is impossible */
case class CastCellToSubtypeChecked(result : TempValue, value : TempValue, toType : ct.CellType, errorMessage : RuntimeErrorMessage) extends Step

/** Casts a cell to another type without checking the validity of the cast */
case class CastCellToTypeUnchecked(result : TempValue, value : TempValue, toType : ct.CellType) extends Step

/** Converts an native integer to another width and/or signedness */
case class ConvertNativeInteger(result : TempValue, fromValue : TempValue, toBits : Int, signed : Boolean) extends Step

/** Converts an native float to another type */
case class ConvertNativeFloat(result : TempValue, fromValue : TempValue, toType : vt.FpType) extends Step
/** Converts an native integer to a float */
case class ConvertNativeIntegerToFloat(result : TempValue, fromValue : TempValue, fromSigned: Boolean, toType : vt.FpType) extends Step
      
/** Builds a proper list at runtime
  *
  * @param result      location to store the head of the proper list
  * @param allocation  allocation to allocate from
  * @param allocIndex  offset in the allocation to allocate from
  * @param listValues  DatumCell values to add to the list
  */
case class BuildProperList(result : TempValue, allocation : TempAllocation, allocIndex : Int, listValues : List[TempValue]) extends Step

/** Indicates a step that stores a constant value */
sealed trait StoreConstant extends Step {
  val result : TempValue
}

/** Stores an entry point with the given signature and native symbol
  *
  * This can be called with Invoke
  */
case class StoreNamedEntryPoint(result : TempValue, signature : ProcedureSignature, nativeSymbol : String) extends Step

/** Indicates a step that stores a constant cell */
sealed trait StoreConstantCell extends StoreConstant 

case class StoreStringCell(result : TempValue, value : String) extends StoreConstantCell
case class StoreSymbolCell(result : TempValue, value : String) extends StoreConstantCell
case class StoreExactIntegerCell(result : TempValue, value : Long) extends StoreConstantCell
case class StoreInexactRationalCell(result : TempValue, value : Double) extends StoreConstantCell
case class StoreCharacterCell(result : TempValue, value : Char) extends StoreConstantCell
case class StoreBooleanCell(result : TempValue, value : Boolean) extends StoreConstantCell
case class StoreBytevectorCell(result : TempValue, elements : Vector[Short]) extends StoreConstantCell
case class StorePairCell(result : TempValue, car : TempValue, cdr : TempValue) extends StoreConstantCell
case class StoreVectorCell(result : TempValue, elements : Vector[TempValue]) extends StoreConstantCell
case class StoreUnitCell(result : TempValue) extends StoreConstantCell
case class StoreEmptyListCell(result : TempValue) extends StoreConstantCell

/** Stores a procedure with an empty closure 
  *
  * This is equalivent to RecordLikeInit(vt.EmptyClosureType] followed by
  * StoreProcedureEntryPoint except it uses a compile time constani cell and 
  * is considerably more efficient
  **/
case class StoreEmptyClosure(result : TempValue, entryPoint : TempValue) extends StoreConstantCell

/** Indicates a step that stores a native constant */
sealed trait StoreNativeConstant extends StoreConstant

case class StoreNativeInteger(result : TempValue, value : Long, bits : Int) extends StoreNativeConstant
case class StoreNativeFloat(result : TempValue, value : Double, fpType : vt.FpType) extends StoreNativeConstant

/** Indicates a step that unboxes a cell */
sealed trait UnboxValue extends Step {
  val result : TempValue
  val boxed : TempValue
}

/** Stores if a cell is "truthy". All values except false are truthy. */
case class UnboxAsTruthy(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxExactInteger(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxInexactRational(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxCharacter(result : TempValue, boxed : TempValue) extends UnboxValue

// These aren't quite an unboxing because there's two values per boxed value

/** Stores the car of the passed PairCell as a DatumCell */
case class StorePairCar(result : TempValue, boxed : TempValue) extends Step
/** Stores the cdr of the passed PairCell as a DatumCell */
case class StorePairCdr(result : TempValue, boxed : TempValue) extends Step

/** Store the entry point of a procedure */
case class StoreProcedureEntryPoint(result : TempValue, boxed : TempValue) extends Step

/** Indicates a step that boxes a native value */
sealed trait BoxValue extends Step {
  val result : TempValue
  val unboxed : TempValue
}

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result : TempValue, unboxed : TempValue) extends BoxValue
case class BoxExactInteger(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxInexactRational(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxCharacter(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue

/** Returns from the current function */
case class Return(returnValue : Option[TempValue]) extends Step

/** Allocates data for a given record a given type 
 *
 * @param cellResult  location to store the record cell 
 * @param dataResult  location to store the uninitialized record data 
 * @param allocation  allocation to allocate the cell from
 * @param allocIndex  offset in the allocation to allocate from
 * @param recordType  type of record to create
 */
case class RecordLikeInit(cellResult : TempValue, dataResult : TempValue, allocation : TempAllocation, allocIndex : Int, recordLikeType : vt.RecordLikeType) extends Step

/** Sets a record field. The value must match the type of record field */
case class RecordDataFieldSet(recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField, newValue : TempValue) extends Step
/** Reads a record field. The value must match the type of record field */
case class RecordDataFieldRef(result : TempValue, recordData : TempValue, recordLikeType : vt.RecordLikeType, recordField : vt.RecordField) extends Step

/** Tests to see if a record is of a given class */
case class TestRecordLikeClass(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step

/** Asserts that a record is of a given class 
  *
  * A runtime error will occur if the record isn't of the passed class
  */
case class AssertRecordLikeClass(recordCell : TempValue, recordLikeType : vt.RecordLikeType, errorMessage : RuntimeErrorMessage) extends Step

/** Stores the data of a record */
case class StoreRecordLikeData(result : TempValue, recordCell : TempValue, recordLikeType : vt.RecordLikeType) extends Step

/** Sets the entry point of a procedure
  *
  * The procedure should be created using RecordLikeInit with a ClosureType
  */
case class SetProcedureEntryPoint(procedureCell : TempValue, entryPoint : TempValue) extends Step
