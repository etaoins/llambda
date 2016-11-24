package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.InternalCompilerErrorException

object NameForReturnType {
  def apply[T >: SchemeType <: ValueType](returnType : ReturnType.ReturnType[T]) : String = returnType match {
    case ReturnType.Unreachable =>
      "<unit> (unreachable)"

    case ReturnType.Reachable(valueType) =>
      NameForType(valueType)
  }
}
