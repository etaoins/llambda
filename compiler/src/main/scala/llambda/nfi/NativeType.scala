package llambda.nfi

import llambda.st

sealed abstract class NativeType {
  val schemeType : Option[st.SchemeType]
}

class IntType(val bits : Integer) extends NativeType {
  val schemeType : Option[st.SchemeType] = Some(st.ExactIntegerType)
}

case object Bool32 extends IntType(32) {
  override val schemeType = Some(st.BooleanType)
}

case object Int8 extends IntType(8)
case object Int16 extends IntType(16)
case object Int32 extends IntType(32)
case object Int64 extends IntType(64)

class FloatingPointType(val bits : Integer) extends NativeType {
  val schemeType = Some(st.RealType)
}

case object Float extends FloatingPointType(32)
case object Double extends FloatingPointType(64)

case object BoxedDatum extends NativeType {
  // We don't know the type here
  val schemeType = None
}

case object Utf8String extends NativeType {
  val schemeType = Some(st.StringType)
}
