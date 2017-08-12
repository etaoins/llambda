package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}

object ConstrainValue {
  /** Represents a constraint that can be applied to an intermediate value */
  sealed abstract class ValueConstraint {
    def applyToSubject(subjectValue: iv.IntermediateValue): iv.IntermediateValue

    /** Returns if this constraint is definitely a no-op
      *
      * This is for optimisation purposes only; it's legal to always return false here
      */
    def definiteNoop: Boolean
  }

  /** Represents a constaint that can be applied to an intermediate value's type */
  sealed abstract class TypeConstraint extends ValueConstraint {
    protected def applyToSubjectType(subjectType: vt.SchemeType): vt.SchemeType

    def applyToSubject(subjectValue: iv.IntermediateValue): iv.IntermediateValue = {
      // Calculate the new type
      val existingType = subjectValue.schemeType
      val rawNewType = applyToSubjectType(existingType)

      // We have to be careful to make storage locations with "stable" types in case they're upcast
      val stableNewType = vt.StabiliseType(rawNewType)

      // Use eq here for speed. This is an optimisation so can miss matching types without affecting correctness.
      if (existingType eq stableNewType) {
        subjectValue
      }
      else {
        // Create a new intermediate value with the constrained type
        subjectValue.withSchemeType(stableNewType)
      }
    }
  }

  /** Intersects the subject value's type with the given type */
  case class IntersectType(withType: vt.SchemeType) extends TypeConstraint {
    override protected def applyToSubjectType(subjectType: vt.SchemeType) =
      subjectType & withType

    def definiteNoop: Boolean =
      withType eq vt.AnySchemeType
  }

  /** Subtracts the given type from the subject value's type */
  case class SubtractType(withType: vt.SchemeType) extends TypeConstraint {
    override protected def applyToSubjectType(subjectType: vt.SchemeType) =
      subjectType - withType

    def definiteNoop: Boolean =
      withType == vt.EmptySchemeType
  }

  /** Substitutes a value with a constant value */
  case class SubstituteWithConstant(withValue: iv.ConstantValue) extends ValueConstraint {
    def applyToSubject(subjectValue: iv.IntermediateValue): iv.IntermediateValue =
      withValue

    def definiteNoop: Boolean =
      false
  }

  /** Preserves the existing value
    *
    * This is useful as a no-op placeholder for either trueConstraint or falseConstraint in addCondAction
    */
 case object PreserveValue extends ValueConstraint {
    def applyToSubject(subjectValue: iv.IntermediateValue): iv.IntermediateValue =
      subjectValue

    def definiteNoop: Boolean =
      true
  }

  /** Action to perform once a conditional value has had it type constrained
    *
    * @param  subjectValue     Value to have it type acted upon
    * @param  trueConstraint   Type constraint to apply if the condition value is definitely false
    * @param  falseCosntraint  Type constraint to apply if the condition value is definitely true-y
    */
  case class CondAction(
      subjectValue: iv.IntermediateValue,
      trueConstraint: ValueConstraint,
      falseConstraint: ValueConstraint
  )

  case class State(
      condActions: Map[iv.IntermediateValue, List[CondAction]] = Map()
  )

  /** Registers a conditional type with the type constraint system
    *
    * This has no immediate effect on the type information in the state. However, if the conditionValue later has
    * its type constrained in such a way that gives it a definite truthiness the appropriate action will be performed
    * on subjectValue's type.
    *
    * Note that this can cause arbitrarily long cascades of type constraints to be applied once one root condition has
    * its type constrained to a definite truthiness.
    *
    * @param  conditionValue   Condition to trigger type constraints on
    * @param  condAction       Action to perform once conditionValue has its type constrained to definite truthiness
    */
  def addCondAction(state: PlannerState)(
      conditionValue: iv.IntermediateValue,
      condAction: CondAction
  ): PlannerState =
    addCondActions(state)(conditionValue, List(condAction))

  def addCondActions(state: PlannerState)(
      conditionValue: iv.IntermediateValue,
      condActions: List[CondAction]
  ): PlannerState = {
    val usefulCondActions = condActions.filter { condAction =>
      !condAction.trueConstraint.definiteNoop || !condAction.falseConstraint.definiteNoop
    }

    // Add this action on to the list of actions for the condition value
    val oldConstraintState = state.valueConstraintState

    val existingActions = oldConstraintState.condActions.getOrElse(conditionValue, Nil)
    val updatedActions = oldConstraintState.condActions + (conditionValue -> (usefulCondActions ++ existingActions))

    val newConstraintState = state.valueConstraintState.copy(
      condActions=updatedActions
    )

    state.copy(valueConstraintState=newConstraintState)
  }

  def condActionsForConditionValue(state: PlannerState)(conditionValue: iv.IntermediateValue): List[CondAction] =
    state.valueConstraintState.condActions.getOrElse(conditionValue, Nil)

  private def triggerCondActions(state: PlannerState)(
      value: iv.IntermediateValue,
      newType: vt.SchemeType
  )(planConfig: PlanConfig): PlannerState = {
    state.valueConstraintState.condActions.get(value) match {
      case Some(condActionList) =>
        vt.SatisfiesType(vt.LiteralBooleanType(false), newType) match {
          case Some(definiteFalse) =>
            condActionList.foldLeft(state) { case (state, condAction) =>
              // Clean ourselves out from the state
              val newConstraintState = state.valueConstraintState.copy(
                state.valueConstraintState.condActions - value
              )

              val newState = state.copy(valueConstraintState=newConstraintState)

              // Apply our conditional action
              if (definiteFalse) {
                apply(newState)(condAction.subjectValue, condAction.falseConstraint)(planConfig)
              }
              else {
                apply(newState)(condAction.subjectValue, condAction.trueConstraint)(planConfig)
              }
            }

          case _ =>
            state
        }

      case _ =>
        state
    }
  }

  /** Modifies the passed state by applying a given type constraint to an intermediate value
    *
    * This will modify any storage locations pointed to the passed intermediate value to point to a duplicate
    * intermediate value with the appropriate type
    */
  def apply(state: PlannerState)(
      value: iv.IntermediateValue,
      constraint: ValueConstraint
  )(planConfig: PlanConfig): PlannerState = {
    // We do some O(n) operations below so abort early
    if (constraint.definiteNoop) {
      return state
    }

    val constrainedValue = constraint.applyToSubject(value)

    if (value eq constrainedValue) {
      return state
    }

    def valueMapper(inputValue: iv.IntermediateValue) =
      if (inputValue eq value) constrainedValue else inputValue

    // Modify our state to point to the new value
    // XXX: This is O(n) with the number of live storage locations or conditional types
    val newValues = state.values.mapValues {
      case ImmutableValue(`value`) =>
        ImmutableValue(constrainedValue)

      case other =>
        other
    }

    val newCondActions = state.valueConstraintState.condActions.map { case (conditionValue, actionList) =>
      val newActionList = actionList.map { action =>
        action.copy(
          subjectValue=valueMapper(action.subjectValue)
        )
      }

      valueMapper(conditionValue) -> newActionList
    }

    val newConstraintState = state.valueConstraintState.copy(
      condActions=newCondActions
    )

    val constrainedState = state.copy(
      values=newValues,
      valueConstraintState=newConstraintState
    )

    // Recursively trigger any conditional actions caused by our new constrained value
    triggerCondActions(constrainedState)(constrainedValue, constrainedValue.schemeType)(planConfig)
  }
}
