package llambda.nfi

import llambda.st

sealed abstract class NativeType {
  val schemeType : Option[st.SchemeType]
}

sealed abstract class IntLikeType(val bits : Int, val signed : Boolean) extends NativeType {
  val schemeType : Option[st.SchemeType] = Some(st.ExactIntegerType)
}

case object Bool extends IntLikeType(1, false) {
  override val schemeType = Some(st.BooleanType)
}

sealed abstract class IntType(bits : Int, signed : Boolean) extends IntLikeType(bits, signed)

case object Int8 extends IntType(8, true)
case object Int16 extends IntType(16, true)
case object Int32 extends IntType(32, true)
case object Int64 extends IntType(64, true)

case object UInt8 extends IntType(8, false)
case object UInt16 extends IntType(16, false)
case object UInt32 extends IntType(32, false)
case object UInt64 extends IntType(64, false)

case object Float extends NativeType {
  val schemeType = Some(st.InexactRationalType)
}

case object Double extends NativeType {
  val schemeType = Some(st.InexactRationalType)
}

case object BoxedDatum extends NativeType {
  // We don't know the type here
  val schemeType = None
}

case object Utf8CString extends NativeType {
  val schemeType = Some(st.StringType)
}

case object UnicodeChar extends IntLikeType(32, true) {
  override val schemeType = Some(st.CharType)
}
