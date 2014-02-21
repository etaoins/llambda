package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.planner.{step => ps}

/** Trait for IntermediateValues that cannot be invokable */
trait UninvokableValue extends IntermediateValue {
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[InvokableProcedure] = None
}

