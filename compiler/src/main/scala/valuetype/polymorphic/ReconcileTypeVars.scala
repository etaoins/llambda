package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import io.llambda.compiler.valuetype._

import io.llambda.compiler.{NoSourceLocation, SourceLocated}
import io.llambda.compiler.ImpossibleTypeConversionException

object ReconcileTypeVars {
  case class Result(values : Map[TypeVar, SchemeType] = Map())

  /** Reconciles resolved type variables with their defined upper bounds
    *
    * @param  typeVars        Defined type variables for the polymorphic type
    * @param  resolved        Resolved types variable as determined by ResolveTypeVars. If this is omitted then the
    *                         bounds for all of the defined type variables will be returned.
    * @param  located         Source location causing the reconciliation. This is used to locate any exceptions.
    * @param  strictBounds    Indicates if an exception should be raised if a type bound is violated. Otherwise the
    *                         type variable will default to its upper bound
    * @param  fixApplicable   Indicates if the applicable type of the upper bound should be used. This is important
    *                         for preserving ABI compatibility if a procedure is either accepting or returning a
    *                         certain applicable type.
    */
  def apply(
      typeVars : Set[TypeVar],
      resolved : ResolveTypeVars.Result = ResolveTypeVars.Result(),
      located : SourceLocated = NoSourceLocation,
      strictBounds : Boolean = false,
      fixApplicable : Boolean = false
  ) : ReconcileTypeVars.Result = {
    val finalValues = typeVars.map({ typeVar =>
      val upperBound = typeVar.upperBound

      resolved.values.get(typeVar) match {
        case Some(resolvedType) =>
          if (SatisfiesType(upperBound, resolvedType) != Some(true)) {
            if (strictBounds) {
              throw new ImpossibleTypeConversionException(
                located,
                s"Unable to instantiate polymorphic type; type variable ${typeVar.sourceName} is set to ${resolvedType} which violates its upper type bound of ${upperBound}"
              )
            }
            else {
              // Use the upper bound
              (typeVar -> upperBound)
            }
          }
          else if (fixApplicable) {
            val upperBoundApplicableTypeOpt = upperBound.applicableTypeOpt

            upperBoundApplicableTypeOpt match {
              case Some(applicableType) =>
                (typeVar -> resolvedType.replaceApplicableType(applicableType))

              case _ =>
                (typeVar -> resolvedType)
            }
          }
          else {
            (typeVar -> resolvedType)
          }

        case None =>
          // Just use the upper bound
          (typeVar -> upperBound)
      }
    }).toMap

    Result(finalValues)
  }
}
