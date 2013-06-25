package llambda.nfi

import llambda.st

sealed abstract class NativeType {
  val schemeType : Option[st.SchemeType]
}

class IntLikeType(val bits : Integer) extends NativeType {
  val schemeType : Option[st.SchemeType] = Some(st.ExactIntegerType)
}

case object Bool32 extends IntLikeType(32) {
  override val schemeType = Some(st.BooleanType)
}

abstract class IntType(bits : Integer) extends IntLikeType(bits)

case object Int8 extends IntType(8)
case object Int16 extends IntType(16)
case object Int32 extends IntType(32)
case object Int64 extends IntType(64)

case object Float extends NativeType {
  val schemeType = Some(st.RealType)
}

case object Double extends NativeType {
  val schemeType = Some(st.RealType)
}

case object BoxedDatum extends NativeType {
  // We don't know the type here
  val schemeType = None
}

case object Utf8String extends NativeType {
  val schemeType = Some(st.StringType)
}
