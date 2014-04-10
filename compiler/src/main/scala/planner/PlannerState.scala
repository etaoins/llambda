package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

sealed trait LocationValue
case class ImmutableValue(intermediateValue : iv.IntermediateValue) extends LocationValue
case class MutableValue(mutableTemp : ps.TempValue) extends LocationValue

case class PlannerState(
  values : Map[StorageLocation, LocationValue] = Map(),
  worldPtr : ps.WorldPtrValue
) {
  def withValue(newValue : (StorageLocation, LocationValue)) =
    this.copy(values=values + newValue)
}
