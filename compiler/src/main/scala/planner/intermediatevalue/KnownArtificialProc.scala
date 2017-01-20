package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{InternalCompilerErrorException, PolymorphicSignature}

import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}

abstract class KnownArtificialProc(polySignature: PolymorphicSignature) extends KnownProc(polySignature, None) {
  private var plannedNativeSymbolOpt: Option[String] = None

  /** Hint for the name of the symbol
    *
    * If this name is already taken a unique name will be assigned
    */
  protected val symbolHint: String

  def nativeSymbol(implicit plan: PlanWriter): String = {
    if (!plannedNativeSymbolOpt.isDefined) {
      val allocedSymbol = plan.allocSymbol(symbolHint)
      plan.plannedFunctions += (allocedSymbol -> planFunction(plan, allocedSymbol))

      plannedNativeSymbolOpt = Some(allocedSymbol)
    }

    plannedNativeSymbolOpt.get
  }

  protected def planFunction(parentPlan: PlanWriter, allocedSymbol: String): PlannedFunction

  override def withSelfTemp(selfTemp: ps.TempValue) = {
    // We have no self value so we don't need be to captured and therefore restored
    throw new InternalCompilerErrorException("Attempt to change the self value of an artificial procedure")
  }
}
