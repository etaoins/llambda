package llambda.valuetype

import llambda.nfi
import llambda.sst
import llambda.{boxedtype => bt}

sealed abstract class ValueType
sealed abstract trait IntrinsicType extends ValueType

case class ScalarType(nativeType : nfi.NativeType) extends IntrinsicType
case class BoxedIntrinsicType(boxedType : bt.BoxedType) extends IntrinsicType

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type 
  * can be distinct if it's declared in another record type. It's even possible
  * for one type to have fields with the same source name if they come from
  * different scopes.
  */
final class RecordField(val sourceName : String, val fieldType : ValueType)

/** Uniquely identifies a record type even if has the same name and internal
  * structure as another type 
  */
class BoxedRecordType(val sourceName : String, val fields : List[RecordField]) extends ValueType
