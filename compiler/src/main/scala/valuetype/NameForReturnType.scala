package io.llambda.compiler.valuetype
import io.llambda

object NameForReturnType {
  def apply[T >: SchemeType <: ValueType](returnType: ReturnType.ReturnType[T]): String = returnType match {
    case ReturnType.Unreachable =>
      "<unit> (unreachable)"

    case ReturnType.Reachable(valueType) =>
      NameForType(valueType)
  }
}
