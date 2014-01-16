package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.sst
import llambda.compiler.{celltype => ct}

/** Type of any value known to the compiler
  * 
  * These are more specific than the types defined in R7RS in that they define
  * both the semantic type and the runtime representation. For example, Int32
  * and IntrinsicCellType(ct.ExactIntegerCell) are both exact integers but have
  * different native representations.
  */
sealed abstract class ValueType {
  val schemeName : String
  val isGcManaged : Boolean
}

/** Type represented by a native pointer */
sealed abstract trait PointerType extends ValueType

/** Pointer to a garbage collected value cell */
sealed abstract trait CellValueType extends PointerType {
  val isGcManaged = true
}

/** Intrinsic types known by the compiler backend
  *
  * Note that some types that seem builtin might in fact be based off other
  * types in the frontend, typically using records. For example, the SRFI 111
  * "box" type is implemented as a single field record. These are not consider
  * intrinsic as the backend does not distinguish them from user-defined types.
  */
sealed abstract trait IntrinsicType extends ValueType

/** Primitive types shared with C */
sealed abstract class NativeType extends IntrinsicType {
  val isGcManaged = false
}

/** Type reperesented by a native integer */
sealed abstract class IntLikeType(val bits : Int, val signed : Boolean) extends NativeType

/** C99/C++ style single byte boolean */
case object CBool extends IntLikeType(8, false) {
  val schemeName = "<bool>"
}

/** Native integer type representing a Scheme exact integer */
sealed abstract class IntType(bits : Int, signed : Boolean) extends IntLikeType(bits, signed) {
  val schemeName = if (signed) {
    s"<int${bits}>"
  }
  else {
    s"<uint${bits}>"
  }
}

case object Int8 extends IntType(8, true)
case object Int16 extends IntType(16, true)
case object Int32 extends IntType(32, true)
case object Int64 extends IntType(64, true)

case object UInt8 extends IntType(8, false)
case object UInt16 extends IntType(16, false)
case object UInt32 extends IntType(32, false)
// UInt64 is outside the range we can represent

/** Native floating point type representing a Scheme inexact rational */
sealed abstract class FpType extends NativeType

case object Float extends FpType {
  val schemeName = "<float>"
}

case object Double extends FpType {
  val schemeName = "<double>"
}

/** Native integer representing a Unicode code point */
case object UnicodeChar extends IntLikeType(32, true) {
  val schemeName = "<unicode-char>"
}

/** Pointer to a garbage collected value cell containing an intrinsic type */
case class IntrinsicCellType(cellType : ct.CellType) extends IntrinsicType with CellValueType {
  val schemeName = cellType.schemeName
}

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type 
  * can be distinct if it's declared in another record type. It's even possible
  * for one type to have fields with the same source name if they come from
  * different scopes.
  */
class RecordField(val sourceName : String, val fieldType : ValueType)

/** Pointer to a garabge collected value cell containing a record-like type */
sealed abstract class RecordLikeType extends CellValueType {
  val sourceName : String
  val fields : List[RecordField]
  val cellType : ct.ConcreteCellType with ct.RecordLikeFields
}

/** Pointer to a garabge collected value cell containing a user-defined record type
  * 
  * This uniquely identifies a record type even if has the same name and internal
  * structure as another type 
  */
class RecordType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType {
  val cellType = ct.RecordCell
  val schemeName = sourceName
}

/** Pointer to a closure type
  *
  * Closure types store the data needed for a procedure from its parent
  * lexical scope. The storage is internally implemented identically to 
  * user-defined record types.
  **/
class ClosureType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType {
  val cellType = ct.ProcedureCell
  val schemeName = "<internal-closure-type>"
}
