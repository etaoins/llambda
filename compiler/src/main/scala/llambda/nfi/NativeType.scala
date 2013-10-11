package llambda.nfi

import llambda.codegen.{boxedtype => bt}

sealed abstract class NativeType

sealed abstract class IntLikeType(val bits : Int, val signed : Boolean) extends NativeType

case object CBool extends IntLikeType(8, false)

sealed abstract class IntType(bits : Int, signed : Boolean) extends IntLikeType(bits, signed)

case object Int8 extends IntType(8, true)
case object Int16 extends IntType(16, true)
case object Int32 extends IntType(32, true)
case object Int64 extends IntType(64, true)

case object UInt8 extends IntType(8, false)
case object UInt16 extends IntType(16, false)
case object UInt32 extends IntType(32, false)
case object UInt64 extends IntType(64, false)

case object Float extends NativeType

case object Double extends NativeType 

case class BoxedValue(boxedType : bt.BoxedType) extends NativeType

case object Utf8CString extends NativeType

case object UnicodeChar extends IntLikeType(32, true)
