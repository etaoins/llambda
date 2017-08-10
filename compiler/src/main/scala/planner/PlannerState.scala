package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}

sealed trait LocationValue
case class ImmutableValue(intermediateValue: iv.IntermediateValue) extends LocationValue
case class MutableValue(mutableType: MutableType, mutableTemp: ps.TempValue, needsUndefCheck: Boolean) extends LocationValue

sealed trait ParameterValue
case class KnownParameterValue(intermediateValue: iv.IntermediateValue) extends ParameterValue
case class KnownParameterType(schemeType: vt.SchemeType) extends ParameterValue

final class ParameterIdentity

case class PlannerState(
  values: Map[StorageLocation, LocationValue] = Map(),
  parameterValues: Map[ParameterIdentity, ParameterValue] = Map(),
  valueConstraintState: ConstrainValue.State = ConstrainValue.State(),
  inlineDepth: Int = 0
) {
  def withValues(newValues: Map[StorageLocation, LocationValue]): PlannerState  =
    this.copy(values=values ++ newValues)

  def withValue(newValue: (StorageLocation, LocationValue)): PlannerState  =
    this.copy(values=values + newValue)
}
