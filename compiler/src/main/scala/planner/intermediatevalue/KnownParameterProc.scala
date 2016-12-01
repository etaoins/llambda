package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner._
import llambda.compiler.ContextLocated
import llambda.compiler.codegen

/** Represents a known parameter procedure created by (make-parameter)
  *
  * This is used to optimise (parameterize) and applications of the parameter procedure
  *
  * @param  selfTemp             Parameter procedure cell
  * @param  initialValue         Initial value of the parameter. This is used for optimisation if the program does not
  *                              use (parameterize)
  * @param  initialValueInScope  Indicates if initialValue is still is scope. This can be used to avoid reloading the
  *                              value from the parameter procedure.
  */
class KnownParameterProc(
    selfTemp : ps.TempValue,
    initialValue : IntermediateValue,
    initialValueInScope : Boolean = false
) extends KnownProc(
    codegen.RuntimeFunctions.valueForParameterSignature.toPolymorphic,
    Some(selfTemp)
) {
  override val typeDescription = "parameter procedure"

  def nativeSymbol(implicit plan : PlanWriter) =
    codegen.RuntimeFunctions.valueForParameter.name

  def withStdlibName(newStdlibName : String) : KnownParameterProc =
    new KnownParameterProc(selfTemp, initialValue, initialValueInScope)

  def withSelfTemp(selfTemp : ps.TempValue) =
    new KnownParameterProc(selfTemp, initialValue, initialValueInScope=false)

  override def toBoxedValue()(implicit plan : PlanWriter) : BoxedValue =
    BoxedValue(ct.ProcedureCell, selfTemp)

  override def attemptInlineApplication(state : PlannerState)(
      args : List[(ContextLocated, IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[PlanResult] = {
    val constantParameters = !plan.config.analysis.parameterized && plan.config.optimise

    args match {
      case Nil =>
        if (constantParameters && (initialValueInScope || !initialValue.needsClosureRepresentation)) {
          return Some(PlanResult(
            state=state,
            value=initialValue
          ))
        }

        // Load this parameter value without going through the parameter procedure's trampoline
        val resultTemp = ps.Temp(vt.AnySchemeType)
        plan.steps += ps.LoadValueForParameterProc(resultTemp, selfTemp)

        val resultType = if (constantParameters) {
          initialValue.schemeType
        }
        else {
          vt.AnySchemeType
        }

        val resultValue = new CellValue(resultType, BoxedValue(ct.AnyCell, resultTemp))

        Some(PlanResult(
          state=state,
          value=resultValue
        ))

      case _ =>
        None
    }
  }
}
