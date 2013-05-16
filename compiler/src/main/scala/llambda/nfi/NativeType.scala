package llambda.nfi

sealed abstract class NativeType

class IntType(val bits : Integer) extends NativeType

case object Bool extends IntType(1)
case object Int8 extends IntType(8)
case object Int16 extends IntType(16)
case object Int32 extends IntType(32)
case object Int64 extends IntType(64)

class FloatingPointType(val bits : Integer) extends NativeType

case object Float extends FloatingPointType(32)
case object Double extends FloatingPointType(64)

case object BoxedDatum extends NativeType
