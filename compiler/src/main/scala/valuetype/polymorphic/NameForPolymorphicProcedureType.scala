package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

object NameForPolymorphicProcedureType {
  private def nameForTypeVar(typeVar : TypeVar) : String = typeVar.upperBound match {
    case AnySchemeType =>
      typeVar.sourceName

    case nonDefaultBound =>
      "[" + typeVar.sourceName  + " : " + NameForType(nonDefaultBound) + "]"
  }

  /** Returns the Scheme name for the passed polymorphic procedure type
    *
    * This is implemented using [[NameForType]]. The same caveats apply.
    */
  def apply(polyType : PolymorphicProcedureType) : String = {
    if (polyType.typeVars.isEmpty) {
      // Not actually polymorphic
      NameForType(polyType.template)
    }
    else {
      val typeVarDecl = polyType.typeVars.map(nameForTypeVar).mkString(" ")
      s"(All (${typeVarDecl}) ${NameForType(polyType.template)})"
    }
  }
}
