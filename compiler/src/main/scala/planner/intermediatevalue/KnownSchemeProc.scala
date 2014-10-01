package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, StorageLocation, ContextLocated}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.et

/** Represents a user-provided procedure with a Scheme language definitio */
class KnownSchemeProc(
    signature : ProcedureSignature,
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    val parentState : PlannerState,
    val lambdaExpr : et.Lambda,
    val recursiveSelfLoc : Option[StorageLocation],
    reportNameOpt : Option[String] = None)
extends KnownUserProc(signature, plannedSymbol, selfTempOpt, reportNameOpt) {
  // Override this to ensure we have vt.ProcedureType
  // This is required for KnownCaseLambdaProc to collect its type from its clauses
  override val schemeType : vt.ProcedureType = signature.toSchemeProcedureType

  override def locationOpt : Option[ContextLocated] =
    Some(lambdaExpr)

  override def withReportName(newReportName : String) : KnownUserProc = {
    new KnownSchemeProc(
      signature,
      plannedSymbol,
      selfTempOpt,
      parentState,
      lambdaExpr,
      recursiveSelfLoc,
      Some(newReportName)
    )
  }
  
  override def withSelfTemp(selfTemp : ps.TempValue) : KnownUserProc =
    new KnownSchemeProc(
      signature,
      plannedSymbol,
      Some(selfTemp),
      parentState,
      lambdaExpr,
      recursiveSelfLoc,
      reportNameOpt
    )
}

