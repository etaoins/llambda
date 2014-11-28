package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

object InstantiateType {
  private def instantiateSchemeType(poly : SchemeType, reconciledVars : ReconcileTypeVars.Result) : SchemeType = {
    reconciledVars.values.zipWithIndex.foldLeft(poly) { case (poly, (varType, index)) =>
      // Because type variables are represented by recursive type references we can {ab,re}use the UnrollType
      // infrastructure to perform our replacement
      UnrollType.unrollType(poly, varType, index + 1)
    }
  }

  /** Instantiates a type by replacing any internal type variables with their reconciled types */
  def apply[T >: SchemeType <: ValueType](poly : T, reconciledVars : ReconcileTypeVars.Result) : T = poly match {
    case schemeType : SchemeType =>
      instantiateSchemeType(schemeType, reconciledVars)

    case other =>
      // This is for procedure signature which can be both polymorphic and contain native types
      // Native types cannot contain type variables so just pass it through directly
      other
  }
}
