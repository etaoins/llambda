package llambda.planner.step

import llambda.nfi
import llambda.ast
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}

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
case class Invoke(result : Option[TempValue], signature : nfi.NativeSignature, entryPoint : TempValue, arguments : List[TempValue]) extends Step with GcBarrier

/** Allocates a given number of Cons cells at runtime */
case class AllocateCons(result : TempAllocation, count : Int) extends Step with GcBarrier

/** Initializes a mutable variable from an allocation 
  *
  * It's contents are initially undefined. It is an error to reference the
  * variable or enter the GC without setting the variable with MutableVarSet
  */
case class MutableVarInit(result : TempValue, allocation : TempAllocation, allocIndex : Int) extends Step

/** Sets a mutable variable to the passed BoxedDatum value */
case class MutableVarSet(mutable : TempValue, newValue : TempValue) extends Step

/** Returns the contents of a mutable variable as a BoxedDatum */
case class MutableVarRef(result : TempValue, mutable : TempValue) extends Step

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

/** Tests if a boxed value is of a given type */
case class TestBoxedType(result : TempValue, value : TempValue, testType : bt.ConcreteBoxedType) extends Step

/** Casts a boxed value to a subtype aborting if the cast is impossible */
case class CastBoxedToSubtypeChecked(result : TempValue, value : TempValue, toType : bt.BoxedType) extends Step

/** Casts a boxed value to another type without checking the validity of the cast */
case class CastBoxedToTypeUnchecked(result : TempValue, value : TempValue, toType : bt.BoxedType) extends Step

/** Converts an unboxed integer to another width and/or signedness */
case class ConvertNativeInteger(result : TempValue, fromValue : TempValue, toBits : Int, signed : Boolean) extends Step

/** Converts an unboxed float to another type */
case class ConvertNativeFloat(result : TempValue, fromValue : TempValue, toType : nfi.FpType) extends Step
/** Converts an unboxed integer to a float */
case class ConvertNativeIntegerToFloat(result : TempValue, fromValue : TempValue, fromSigned: Boolean, toType : nfi.FpType) extends Step
      
/** Builds a proper list at runtime
  *
  * @param result      location to store the head of the proper list
  * @param allocation  allocation to allocate from
  * @param allocIndex  offset in the allocation to allocate from
  * @param listValues  BoxedDatum values to add to the list
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
case class StoreNamedEntryPoint(result : TempValue, signature : nfi.NativeSignature, nativeSymbol : String) extends Step

/** Indicates a step that stores a boxed constant */
sealed trait StoreBoxedConstant extends StoreConstant 

case class StoreBoxedString(result : TempValue, value : String) extends StoreBoxedConstant
case class StoreBoxedSymbol(result : TempValue, value : String) extends StoreBoxedConstant
case class StoreBoxedExactInteger(result : TempValue, value : Long) extends StoreBoxedConstant
case class StoreBoxedInexactRational(result : TempValue, value : Double) extends StoreBoxedConstant
case class StoreBoxedCharacter(result : TempValue, value : Char) extends StoreBoxedConstant
case class StoreBoxedBoolean(result : TempValue, value : Boolean) extends StoreBoxedConstant
case class StoreBoxedBytevector(result : TempValue, elements : Vector[Short]) extends StoreBoxedConstant
case class StoreBoxedPair(result : TempValue, car : TempValue, cdr : TempValue) extends StoreBoxedConstant
case class StoreBoxedVector(result : TempValue, elements : Vector[TempValue]) extends StoreBoxedConstant
case class StoreBoxedUnspecific(result : TempValue) extends StoreBoxedConstant
case class StoreBoxedEmptyList(result : TempValue) extends StoreBoxedConstant

/** Indicates a step that stores an unboxed constant */
sealed trait StoreNativeConstant extends StoreConstant

case class StoreNativeUtf8String(result : TempValue, value : String) extends StoreNativeConstant
case class StoreNativeInteger(result : TempValue, value : Long, bits : Int) extends StoreNativeConstant
case class StoreNativeFloat(result : TempValue, value : Double, fpType : nfi.FpType) extends StoreNativeConstant

/** Indicates a step that unboxes a boxed value */
sealed trait UnboxValue extends Step {
  val result : TempValue
  val boxed : TempValue
}

/** Stores if a boxed value is "truthy". All values except false are truthy. */
case class UnboxAsTruthy(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxExactInteger(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxInexactRational(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxCharacter(result : TempValue, boxed : TempValue) extends UnboxValue
case class UnboxStringAsUtf8(result : TempValue, boxed : TempValue) extends UnboxValue

// These aren't quite an unboxing because there's two values per boxed value

/** Stores the car of the passed BoxedPair as a BoxedDatum */
case class StorePairCar(result : TempValue, boxed : TempValue) extends Step
/** Stores the cdr of the passed BoxedPair as a BoxedDatum */
case class StorePairCdr(result : TempValue, boxed : TempValue) extends Step

/** Store the closure of a procedure */
case class StoreProcedureClosure(result : TempValue, boxed : TempValue) extends Step
/** Store the entry point of a procedure */
case class StoreProcedureEntryPoint(result : TempValue, boxed : TempValue) extends Step

/** Indicates a step that boxes an unboxed value */
sealed trait BoxValue extends Step {
  val result : TempValue
  val unboxed : TempValue
}

/** Boxes an i8 that's either 0 or 1 as a boolean */
case class BoxBoolean(result : TempValue, unboxed : TempValue) extends BoxValue
case class BoxExactInteger(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxInexactRational(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxCharacter(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxUtf8String(result : TempValue, unboxed : TempValue) extends BoxValue with GcBarrier
case class BoxProcedure(result : TempValue, allocation : TempAllocation, allocIndex : Int, unboxed : TempValue) extends BoxValue
case class BoxRecord(result : TempValue, allocation : TempAllocation, allocIndex : Int, recordType : vt.BoxedRecordType, unboxed : TempValue) extends BoxValue

/** Returns from the current function */
case class Return(returnValue : Option[TempValue]) extends Step

/** Allocates a record of a give type */
case class RecordAllocate(result : TempValue, recordType : vt.BoxedRecordType) extends Step
/** Sets a record field. The value must match the type of record field */
case class RecordFieldSet(recordData : TempValue, recordType : vt.BoxedRecordType, recordField : vt.RecordField, newValue : TempValue) extends Step
/** Reads a record field. The value must match the type of record field */
case class RecordFieldRef(result : TempValue, recordData : TempValue, recordType : vt.BoxedRecordType, recordField : vt.RecordField) extends Step

/** Tests to see if a record is of a given class */
case class TestBoxedRecordClass(result : TempValue, boxedRecord : TempValue, recordType : vt.BoxedRecordType) extends Step
/** Asserts that a record is of a given class */
case class AssertBoxedRecordClass(boxedRecord : TempValue, recordType : vt.BoxedRecordType) extends Step
/** Stores the data of a record */
case class StoreBoxedRecordData(result : TempValue, boxedRecord : TempValue, recordType : vt.BoxedRecordType) extends Step
