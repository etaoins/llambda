package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

import io.llambda.compiler.{NoSourceLocation, SourceLocated}
import io.llambda.compiler.ImpossibleTypeConversionException

object ReconcileTypeVars {
  case class Result(values : Map[TypeVar, SchemeType] = Map())

  /** Reconciles resolved type variables with their defined upper bounds
    *
    * If a resolved type variable violated the defined upper bound then an ImpossibleTypeConversionException will be
    * thrown. If any resolved variables are missing they are assumed to be equal to the defined upper bound.
    *
    * @param  typeVars  Defined type variables for the polymorphic type
    * @param  located   Source location causing the reconciliation. This is used to locate any exceptions.
    * @param  resolved  Resolved types variable as determined by ResolveTypeVars. If this is omitted then the upper
    *                   bounds for all of the defined type variables will be returned.
    */
  def apply(
      typeVars : Set[TypeVar],
      located : SourceLocated = NoSourceLocation,
      resolved : ResolveTypeVars.Result = ResolveTypeVars.Result()
  ) : ReconcileTypeVars.Result = {
    val finalValues = typeVars.map({ typeVar =>
      val upperBound = typeVar.upperBound

      resolved.values.get(typeVar) match {
        case Some(resolvedType) =>
          if (SatisfiesType(upperBound, resolvedType) != Some(true)) {
            throw new ImpossibleTypeConversionException(
              located,
              s"Unable to instantiate polymorphic type; resolved type variable ${resolvedType} violates upper type bound of ${upperBound}"
            )
          }

          (typeVar -> resolvedType)

        case None =>
          // Just use the upper bound
          (typeVar -> upperBound)
      }
    }).toMap

    Result(finalValues)
  }
}
