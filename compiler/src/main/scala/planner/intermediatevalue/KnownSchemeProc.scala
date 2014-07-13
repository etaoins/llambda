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
  
  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownSchemeProc(
      signature,
      plannedSymbol,
      Some(varTemp),
      parentState,
      lambdaExpr,
      recursiveSelfLoc,
      reportNameOpt
    )
  }
}

