package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.dialect
import llambda.compiler.{celltype => ct}

object StabiliseReturnType {
  /** Stablises the type of a return type
    *
    * This is a simple wrapper around [[StabliseType]] to work on [[ReturnType]] instancds
    */
  def apply(
      returnType : ReturnType.ReturnType[SchemeType],
      schemeDialect : dialect.Dialect
  ) : ReturnType.ReturnType[SchemeType] = returnType match {
    case ReturnType.UnreachableValue =>
      ReturnType.UnreachableValue

    case ReturnType.SingleValue(valueType) =>
      ReturnType.SingleValue[SchemeType](StabiliseType(valueType, schemeDialect))

    case ReturnType.MultipleValues(valueType) =>
      ReturnType.MultipleValues(StabiliseType(valueType, schemeDialect))
  }
}
