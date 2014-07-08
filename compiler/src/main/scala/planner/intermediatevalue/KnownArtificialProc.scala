package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{InternalCompilerErrorException, ProcedureSignature}

import llambda.compiler.planner._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}

abstract class KnownArtificialProc extends KnownProc(None) {
  private var plannedNativeSymbolOpt : Option[String] = None

  /** Hint for the name of the symbol
    *
    * If this name is already taken a unique name will be assigned 
    */
  protected val symbolHint : String

  /** Signature for the procedure
    *
    * This is needed before the procedure is actually planned to support function typing in the future
    */
  val signature : ProcedureSignature

  def nativeSymbol(implicit plan : PlanWriter) : String = {
    if (!plannedNativeSymbolOpt.isDefined) {
      val allocedSymbol = plan.allocProcedureSymbol(symbolHint)
      plan.plannedFunctions += (allocedSymbol -> planFunction(plan))

      plannedNativeSymbolOpt = Some(allocedSymbol)
    }

    plannedNativeSymbolOpt.get
  }
  
  protected def planFunction(parentPlan : PlanWriter) : PlannedFunction

  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    // We have no self value so we don't need be to captured and therefore restored
    throw new InternalCompilerErrorException("Attempted to restore an artificial procedure from a closure")
  }

}
