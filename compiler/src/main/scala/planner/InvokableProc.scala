package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.planner.{step => ps}


case class InvokableProc(
  signature: ProcedureSignature,
  entryPointTemp: ps.TempValue,
  selfTempOpt: Option[ps.TempValue]
)
