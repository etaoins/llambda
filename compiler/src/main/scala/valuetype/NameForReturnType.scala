package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.InternalCompilerErrorException

object NameForReturnType {
  def apply(returnType : ReturnType.ReturnType) : String = returnType match {
    case ReturnType.SingleValue(valueType) =>
      NameForType(valueType)

    case ReturnType.ArbitraryValues =>
      "*"

    case ReturnType.MultipleValues(SpecificProperListType(typeRefs)) =>
      val typeNames = typeRefs map {
        case DirectSchemeTypeRef(schemeType) =>
          NameForType(schemeType)

        case _ =>
          throw new InternalCompilerErrorException("Recursive return types are unsupported")
      }

      "(" + ("values" :: typeNames).mkString(" ") + ")"

    case ReturnType.MultipleValues(otherListType) =>
      // We have no representation for this
      "?" + NameForType(otherListType) + "?"
  }
}
