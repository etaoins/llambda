package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._

import llambda.compiler.{NoSourceLocation, SourceLocated}
import llambda.compiler.TypeException

object ReconcileTypeVars {
  case class Result(values: Map[TypeVar, SchemeType] = Map())

  /** Reconciles resolved type variables with their defined upper bounds
    *
    * @param  typeVars       Defined type variables for the polymorphic type
    * @param  resolved       Resolved types variable as determined by ResolveTypeVars. If this is omitted then the
    *                        bounds for all of the defined type variables will be returned.
    * @param  located        Source location causing the reconciliation. This is used to locate any exceptions.
    * @param  strictBounds   Indicates if an exception should be raised if a type bound is violated. Otherwise the
    *                        type variable will default to its upper bound
    * @param  fixProcedure   Indicates if the proceduer type of the upper bound should be used. This is important for
    *                        preserving ABI compatibility if a procedure is either accepting or returning a certain
    *                        procedure type.
    */
  def apply(
      typeVars: Set[TypeVar],
      resolved: ResolveTypeVars.Result = ResolveTypeVars.Result(),
      located: SourceLocated = NoSourceLocation,
      strictBounds: Boolean = false,
      fixProcedure: Boolean = false
  ): ReconcileTypeVars.Result = {
    val finalValues = typeVars.map({ typeVar =>
      val upperBound = typeVar.upperBound

      resolved.values.get(typeVar) match {
        case Some(resolvedType) =>
          if (SatisfiesType(upperBound, resolvedType) != Some(true)) {
            if (strictBounds) {
              throw new TypeException(
                located,
                s"Unable to instantiate polymorphic type; type variable ${typeVar.sourceName} is set to ${resolvedType} which violates its upper type bound of ${upperBound}"
              )
            }
            else {
              // Use the upper bound
              (typeVar -> upperBound)
            }
          }
          else if (fixProcedure) {
            val upperBoundProcedureTypeOpt = upperBound.procedureTypeOpt

            upperBoundProcedureTypeOpt match {
              case Some(procedureType) =>
                (typeVar -> resolvedType.replaceProcedureType(procedureType))

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
