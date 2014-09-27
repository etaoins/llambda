package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._

/** Represents a user-provided procedure with a known signature and direct entry point
  *
  * @param plannedClauses  List of planned Scheme procedures in the order they appear in the (case-lambda)
  * @param plannedSymbol   Native symbol of the direct entry point to the procedure
  * @param selfTempOpt     For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                        point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                        if this value is explicitly converted to a ct.ProcedureCell
  * @param reportNameOpt   Name of this procedure in R7RS. This is used as a tag to implement certain optimizations
  *                        elsewhere in the planner. It is not directly used by this class
  */
class KnownCaseLambdaProc(
    plannedClauses : List[KnownSchemeProc],
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    reportNameOpt : Option[String] = None
) extends KnownUserProc(CaseLambdaSignature, plannedSymbol, selfTempOpt, reportNameOpt) {
  override def withReportName(newReportName : String) : KnownUserProc = {
    new KnownCaseLambdaProc(plannedClauses, plannedSymbol, selfTempOpt, Some(newReportName))
  }
  
  override def withSelfTemp(selfTemp : ps.TempValue) : KnownUserProc = {
    new KnownCaseLambdaProc(plannedClauses, plannedSymbol, Some(selfTemp), reportNameOpt)
  }
}

