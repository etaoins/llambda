package llambda.planner

import llambda.StorageLocation
import llambda.planner.{step => ps}
import llambda.planner.{intermediatevalue => iv}

case class PlannerState(
  immutables : Map[StorageLocation, iv.IntermediateValue] = Map(),
  mutables : Map[StorageLocation, ps.TempValue] = Map()
) {
  def withImmutable(immutable : (StorageLocation, iv.IntermediateValue)) =
    this.copy(immutables=immutables + immutable)
  
  def withMutable(mutable : (StorageLocation, ps.TempValue)) =
    this.copy(mutables=mutables + mutable)
}
