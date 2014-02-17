package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

case class PlannerState(
  immutables : Map[StorageLocation, iv.IntermediateValue] = Map(),
  mutables : Map[StorageLocation, ps.TempValue] = Map(),
  worldPtr : ps.TempValue
) {
  def withImmutable(immutable : (StorageLocation, iv.IntermediateValue)) =
    this.copy(immutables=immutables + immutable)
  
  def withMutable(mutable : (StorageLocation, ps.TempValue)) =
    this.copy(mutables=mutables + mutable)
}
