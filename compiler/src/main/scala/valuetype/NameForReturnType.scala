package io.llambda.compiler.valuetype


object NameForReturnType {
  def apply[T >: SchemeType <: ValueType](returnType: ReturnType.ReturnType[T]): String = returnType match {
    case ReturnType.Unreachable =>
      "<unit> (unreachable)"

    case ReturnType.Reachable(valueType) =>
      NameForType(valueType)
  }
}
