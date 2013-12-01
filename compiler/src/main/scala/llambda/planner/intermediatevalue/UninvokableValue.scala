package llambda.planner.intermediatevalue

import llambda.planner.{PlanWriter, InvokableProcedure}

/** Trait for IntermediateValues that cannot be invokable */
trait UninvokableValue extends IntermediateValue {
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = None
}

