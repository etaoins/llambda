package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner._
import llambda.compiler.ArityException
import llambda.compiler.ContextLocated
import llambda.compiler.codegen

/** Represents a known parameter procedure created by (make-parameter)
  *
  * This is used to optimise (parameterize) and applications of the parameter procedure
  *
  * @param  selfTemp  Parameter procedure cell
  * @param  identity  Unique identity for this parameter across all of its instances. This is used to look up the value
  *                   or type of the parameter from the state.
  */
class KnownParameterProc(selfTemp: ps.TempValue, val identity: ParameterIdentity) extends KnownProc(
    codegen.RuntimeFunctions.valueForParameterSignature.toPolymorphic,
    Some(selfTemp)
) {
  override val typeDescription = "parameter procedure"

  def nativeSymbol(implicit plan: PlanWriter) =
    codegen.RuntimeFunctions.valueForParameter.name

  def withStdlibName(newStdlibName: String): KnownParameterProc =
    new KnownParameterProc(selfTemp, identity)

  def withSelfTemp(selfTemp: ps.TempValue) =
    new KnownParameterProc(selfTemp, identity)

  override def toBoxedValue()(implicit plan: PlanWriter): BoxedValue =
    BoxedValue(ct.ProcedureCell, selfTemp)

  override def attemptInlineApplication(state: PlannerState)(
      args: List[(ContextLocated, IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = args match {
    case Nil =>
      state.parameterValues.get(identity) match {
        case Some(KnownParameterValue(value)) =>
          Some(PlanResult(state=state, value=value))

        case otherValue =>
          // Load this parameter value without going through the parameter procedure's trampoline
          val resultTemp = ps.TempValue()
          plan.steps += ps.LoadValueForParameterProc(resultTemp, selfTemp)

          val resultType = otherValue match {
            case Some(KnownParameterType(schemeType)) => schemeType
            case _ => vt.AnySchemeType
          }

          val resultValue = new CellValue(resultType, BoxedValue(ct.AnyCell, resultTemp))

          Some(PlanResult(state=state, value=resultValue))
      }

    case _ =>
      throw new ArityException(plan.activeContextLocated, "Parameter procedures don't accept arguments")
  }
}
