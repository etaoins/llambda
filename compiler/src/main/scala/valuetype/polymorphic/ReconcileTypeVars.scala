package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import collection.breakOut

import io.llambda.compiler.valuetype._

import io.llambda.compiler.{NoSourceLocation, SourceLocated}
import io.llambda.compiler.ImpossibleTypeConversionException

object ReconcileTypeVars {
  case class Result(values : Vector[SchemeType] = Vector())

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
      typeVars : Vector[TypeVar],
      located : SourceLocated = NoSourceLocation,
      resolved : ResolveTypeVars.Result = ResolveTypeVars.Result()
  ) : ReconcileTypeVars.Result = {
    val finalValues = typeVars.zipWithIndex.map({ case (TypeVar(upperBound), index : Int) =>
      resolved.values.get(index) match {
        case Some(resolvedType) =>
          if (SatisfiesType(upperBound, resolvedType) != Some(true)) {
            throw new ImpossibleTypeConversionException(
              located,
              s"Unable to instantiate polymorphic type; resolved type variable ${resolvedType} violates upper type bound of ${upperBound}"
            )
          }

          resolvedType

        case None =>
          // Just use the upper bound
          upperBound
      }
    })

    Result(finalValues)
  }
}
