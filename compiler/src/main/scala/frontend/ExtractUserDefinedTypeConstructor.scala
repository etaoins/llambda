package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.valuetype.{polymorphic => pm}

object ExtractUserDefinedTypeConstructor {
  def apply(args : List[sst.ScopedDatum], definition : sst.ScopedDatum) : UserDefinedTypeConstructor = {
    val argTypes = args map ExtractTypeVar

    // Rescope the definition
    val typeBindings = argTypes map { case (name, typeVar) =>
      name -> BoundType(typeVar)
    }

    val scopeMapping = Scope.mappingForBoundValues(typeBindings)
    val rescopedDefinition = definition.rescoped(scopeMapping)

    // Parse the polymorphic type
    val polyType = ExtractType.extractSchemeType(rescopedDefinition)
    UserDefinedTypeConstructor(argTypes.map(_._2), polyType)
  }
}
