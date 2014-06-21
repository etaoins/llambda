package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

class KnownTypePredicateProcedure(signature : ProcedureSignature, nativeSymbol : String, val schemeType : vt.SchemeType, reportName : Option[String] = None) extends KnownProcedure(signature, nativeSymbol, None, reportName) {
  override def withReportName(newReportName : String) : KnownTypePredicateProcedure = {
    new KnownTypePredicateProcedure(signature, nativeSymbol, schemeType, Some(newReportName))
  }

  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownTypePredicateProcedure(signature, nativeSymbol, schemeType, reportName)
  }
}
