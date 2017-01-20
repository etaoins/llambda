package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.PolymorphicSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.ArityException

case class KnownCaseLambdaClause(
    knownProc: KnownSchemeProc,
    recordField: Option[vt.RecordField]
)

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param polySignature   Signature of the outer (case-lambda)
  * @param closureType     Closure type of the outer (case-lambda)
  * @param clauses         List of planned Scheme procedures in the order they appear in the (case-lambda)
  * @param plannedSymbol   Native symbol of the direct entry point to the procedure
  * @param selfTempOpt     For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                        point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                        if this value is explicitly converted to a ct.ProcedureCell
  * @param stdlibNameOpt   Name of this procedure in the stdlib. This is used as a tag to implement certain
  *                        optimisations elsewhere in the planner. It is not directly used by this class.
  * @param clausesInScope  Indicates if the original clause knownProc values are still within scope. This is used as an
  *                        optimisation to avoid having to load them from the (case-lambda)'s closure
  */
class KnownCaseLambdaProc(
    polySignature: PolymorphicSignature,
    closureType: vt.ClosureType,
    clauses: List[KnownCaseLambdaClause],
    plannedSymbol: String,
    selfTempOpt: Option[ps.TempValue],
    stdlibNameOpt: Option[String] = None,
    clausesInScope: Boolean = false
) extends KnownUserProc(polySignature, plannedSymbol, selfTempOpt, stdlibNameOpt) {
  override val typeDescription = "case procedure"

  override val schemeType = vt.CaseProcedureType(
    clauses.map(_.knownProc.schemeType)
  )

  override def withStdlibName(newStdlibName: String): KnownUserProc = {
    val mappedClauses = clauses.map { clause =>
      clause.copy(knownProc=clause.knownProc.withStdlibName(newStdlibName))
    }

    new KnownCaseLambdaProc(
      polySignature=polySignature,
      closureType=closureType,
      clauses=mappedClauses,
      plannedSymbol=plannedSymbol,
      selfTempOpt=selfTempOpt,
      stdlibNameOpt=Some(newStdlibName),
      clausesInScope=clausesInScope
    )
  }

  override def withSelfTemp(selfTemp: ps.TempValue): KnownUserProc =
    new KnownCaseLambdaProc(
      polySignature=polySignature,
      closureType=closureType,
      clauses=clauses,
      plannedSymbol=plannedSymbol,
      selfTempOpt=Some(selfTemp),
      stdlibNameOpt=stdlibNameOpt,
      clausesInScope=false // Our clause values are no longer in scope
    )

  private def restoreClause(
      clause: KnownCaseLambdaClause
  )(implicit plan: PlanWriter): IntermediateValue = {
    clause.recordField match {
      case Some(recordField) if !clausesInScope =>
        val closureDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(closureDataTemp, selfTempOpt.get, closureType)

        val fieldType = closureType.typeForField(recordField)
        val clauseSelfTemp = new ps.TempValue(fieldType.isGcManaged)

        plan.steps += ps.LoadRecordDataField(clauseSelfTemp, closureDataTemp, closureType, recordField)

        clause.knownProc.withSelfTemp(clauseSelfTemp)

      case _ =>
        // We can use the value directly
        clause.knownProc
    }
  }

  /** Returns the clause matching the passed arity or None if no clauses match */
  def clauseForArityOpt(argCount: Int): Option[KnownCaseLambdaClause] =
    clauses find { clause =>
      val signatureTemplate = clause.knownProc.polySignature.template
      val mandatoryArgCount = signatureTemplate.mandatoryArgTypes.length
      val hasRestArg = signatureTemplate.restArgMemberTypeOpt.isDefined

      (argCount == mandatoryArgCount) || ((argCount > mandatoryArgCount) && hasRestArg)
    }

  override def toApplicableValueForArgs (
      args: List[vt.SchemeType]
  )(implicit plan: PlanWriter): IntermediateValue = {
    clauseForArityOpt(args.length) match {
      case Some(clause) =>
        restoreClause(clause)

      case None =>
        val message = s"No (case-lambda) clause matches an arity of ${args.length}"
        throw new ArityException(plan.activeContextLocated, message)
    }
  }
}

