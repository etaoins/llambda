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
  * @param  selfTemp      Parameter procedure cell
  * @param  hasConverter  Indicates if this parameter procedure was created with a converter procedure
  */
class KnownParameterProc(selfTemp : ps.TempValue, val hasConverter : Boolean) extends KnownProc(
    codegen.RuntimeFunctions.valueForParameterSignature,
    Some(selfTemp)
) {
  override val typeDescription = "parameter procedure"
  
  def nativeSymbol(implicit plan : PlanWriter) =
    codegen.RuntimeFunctions.valueForParameter.name

  def withReportName(newReportName : String) : KnownParameterProc =
    new KnownParameterProc(selfTemp, hasConverter)
  
  def withSelfTemp(selfTemp : ps.TempValue) =
    new KnownParameterProc(selfTemp, hasConverter)
  
  override def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue = 
    BoxedValue(ct.ProcedureCell, selfTemp)
  
  override def attemptInlineApplication(state : PlannerState)(
      operands : List[(ContextLocated, IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    operands match {
      case Nil =>
        // Load this parameter value without going through the parameter procedure's trampoline
        val resultTemp = ps.Temp(vt.AnySchemeType)
        plan.steps += ps.LoadValueForParameterProc(worldPtr, resultTemp, selfTemp)

        val resultValue = TempValueToIntermediate(vt.AnySchemeType, resultTemp)(plan.config)

        Some(PlanResult(
          state=state,
          values=SingleValue(resultValue)
        ))

      case _ =>
        None
    }
  }
}
