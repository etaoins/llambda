package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

sealed trait LocationValue
case class ImmutableValue(intermediateValue : iv.IntermediateValue) extends LocationValue
case class MutableValue(mutableType : MutableType, mutableTemp : ps.TempValue, needsUndefCheck : Boolean) extends LocationValue

case class PlannerState(
  values : Map[StorageLocation, LocationValue] = Map(),
  typeConstraintState : ConstrainType.State = ConstrainType.State(),
  inlineDepth : Int = 0
) {
  def withValue(newValue : (StorageLocation, LocationValue)) : PlannerState  =
    this.copy(values=values + newValue)
}
