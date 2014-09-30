package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.IncompatibleArityException

case class KnownCaseLambdaClause(
    knownProc : KnownSchemeProc,
    recordField : Option[vt.RecordField]
)

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param closureType     Closure type of the outer (case-lambda)
  * @param clauses         List of planned Scheme procedures in the order they appear in the (case-lambda)
  * @param plannedSymbol   Native symbol of the direct entry point to the procedure
  * @param selfTempOpt     For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                        point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                        if this value is explicitly converted to a ct.ProcedureCell
  * @param reportNameOpt   Name of this procedure in R7RS. This is used as a tag to implement certain optimizations
  *                        elsewhere in the planner. It is not directly used by this class
  * @param clausesInScope  Indicates if the original clause knownProc values are still within scope. This is used as an
  *                        optimisation to avoid having to load them from the (case-lambda)'s closure
  */
class KnownCaseLambdaProc(
    closureType : vt.ClosureType,
    clauses : List[KnownCaseLambdaClause],
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    reportNameOpt : Option[String] = None,
    clausesInScope : Boolean = false
) extends KnownUserProc(CaseLambdaSignature, plannedSymbol, selfTempOpt, reportNameOpt) {
  override def withReportName(newReportName : String) : KnownUserProc =
    new KnownCaseLambdaProc(
      closureType=closureType,
      clauses=clauses,
      plannedSymbol=plannedSymbol,
      selfTempOpt=selfTempOpt,
      reportNameOpt=Some(newReportName),
      clausesInScope=clausesInScope
    )
  
  override def withSelfTemp(selfTemp : ps.TempValue) : KnownUserProc =
    new KnownCaseLambdaProc(closureType, clauses, plannedSymbol, Some(selfTemp), reportNameOpt, clausesInScope=false)

  private def restoreClause(
      clause : KnownCaseLambdaClause
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : IntermediateValue = {
    clause.recordField match {
      case Some(recordField) if !clausesInScope =>
        val closureDataTemp = ps.RecordLikeDataTemp()
        plan.steps += ps.LoadRecordLikeData(closureDataTemp, selfTempOpt.get, closureType)

        val clauseSelfTemp = new ps.TempValue(recordField.fieldType.isGcManaged)
        plan.steps += ps.LoadRecordDataField(clauseSelfTemp, closureDataTemp, closureType, recordField) 
        
        clause.knownProc.withSelfTemp(clauseSelfTemp) 

      case _ =>
        // We can use the value directly
        clause.knownProc
    }
  }
  
  override def toApplicableValueForArity (
      operandCount : Int
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : IntermediateValue = {
    for(clause <- clauses) {
      val signature = clause.knownProc.signature
      val fixedArgCount = signature.fixedArgTypes.length
      val hasRestArg = signature.restArgMemberTypeOpt.isDefined

      if (operandCount == fixedArgCount) {
        return restoreClause(clause)
      }
      else if ((operandCount > fixedArgCount) && hasRestArg) {
        return restoreClause(clause)
      }
    }

    throw new IncompatibleArityException(plan.activeContextLocated, "No (case-lambda) clause matches the requested arity")
  }
}

