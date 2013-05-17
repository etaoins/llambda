package llambda.st

sealed abstract trait SchemeType

case object StringType extends SchemeType 
case object BooleanType extends SchemeType
case object ExactIntegerType extends SchemeType
case object RealType extends SchemeType
case object SymbolType extends SchemeType
case object EmptyListType extends SchemeType
case object PairType extends SchemeType
case object VectorType extends SchemeType
case object ByteVectorType extends SchemeType
case object CharType extends SchemeType
case object ProcedureType extends SchemeType
case object UnspecificType extends SchemeType

// TODO: Custom types
