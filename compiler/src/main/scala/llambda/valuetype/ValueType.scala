package llambda.valuetype

import llambda.nfi
import llambda.{boxedtype => bt}

sealed abstract class ValueType
sealed abstract trait IntrinsicType extends ValueType

case class ScalarType(nativeType : nfi.NativeType) extends IntrinsicType
case class BoxedValue(boxedType : bt.BoxedType) extends IntrinsicType

/** Unique identifies a record type even if has the same name and internal
  * structure as another type 
  */
final class RecordType(val sourceName : String, val fields : Map[String, ValueType]) extends ValueType
