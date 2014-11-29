package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.valuetype.{polymorphic => pm}

object ExtractUserDefinedTypeConstructor {
  def apply(operands : List[sst.ScopedDatum], definition : sst.ScopedDatum) : UserDefinedTypeConstructor = {
    val operandTypes = operands collect {
      case sst.ScopedProperList(List(
        operandName : sst.ScopedSymbol,
        sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
        upperBoundDatum : sst.ScopedDatum
      )) =>
        // An upper type bound was supplied
        val upperBound = ExtractType.extractSchemeType(upperBoundDatum)
        (operandName -> new pm.TypeVar(operandName.name, upperBound))

      case operandName : sst.ScopedSymbol =>
        (operandName -> new pm.TypeVar(operandName.name))

      case other =>
        val message = s"Unrecognized operand definition. Must be either identiifer or [identifier : <type>]."
        throw new BadSpecialFormException(other, message)
    }

    // Inject the type variables in to scope
    val operandsForScope = operandTypes groupBy(_._1.scope)

    val scopeMapping = operandsForScope map { case (oldScope, scopeArgTypes) =>
      val bindings = collection.mutable.Map(scopeArgTypes.map { case (constructorArgSymbol, valueType) =>
        constructorArgSymbol.name -> (BoundType(valueType) : BoundValue)
      } : _*)

      (oldScope -> new Scope(bindings, Some(oldScope)))
    }

    // Parse the polymorphic type
    val polyType = ExtractType.extractSchemeType(definition.rescoped(scopeMapping))
    UserDefinedTypeConstructor(operandTypes.map(_._2), polyType)
  }
}
