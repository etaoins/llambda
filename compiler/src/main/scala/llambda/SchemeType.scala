package llambda.st

sealed abstract trait SchemeType

case object UnspecificType extends SchemeType
case object StringType extends SchemeType 
case object BooleanType extends SchemeType
case object ExactIntegerType extends SchemeType
case object InexactRationalType extends SchemeType
case object SymbolType extends SchemeType
case object EmptyListType extends SchemeType
case object PairType extends SchemeType
case object VectorType extends SchemeType
case object ByteVectorType extends SchemeType
case object CharType extends SchemeType
case object ProcedureType extends SchemeType

// TODO: Custom types
