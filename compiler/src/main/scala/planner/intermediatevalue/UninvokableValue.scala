package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.ValueNotApplicableException

/** Trait for IntermediateValues that cannot be invokable */
trait UninvokableValue extends IntermediateValue {
  protected def toProcedureTempValue(
      targetType : vt.ProcedureType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val message = errorMessageOpt.map(_.text) getOrElse {
      s"Unable to convert ${typeDescription} to ${targetType}"
    }

    impossibleConversion(message)
  }

  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : InvokableProcedure =
    throw new ValueNotApplicableException(plan.activeContextLocated, typeDescription)
}

