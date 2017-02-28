package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.planner.{step => ps}
import collection.GenTraversableOnce

/** Represents the identity of a GC-managed pointer
  *
  * Aliased pointers created by casts will share the same identity. All other values will GC values will have a unique
  * identity
  */
final class GcPointerIdentity

/** IrValue with an additional flag determining if it is GC managed
  *
  * This is used to determine which values need to be rooted during a GC barrier
  */
case class CollectableIrValue(irValue: IrValue, gcRoot: Boolean)


/** Tracks the state of temp values during codegen
  *
  * When not using pointer aliases this behaves mostly like a simple Map(). However, explicitly aliased values can be
  * added using withAliasedTempValue. Aliasing information is used by the GC logic to remove redundant rooting of
  * aliased values
  */
case class LiveTemps(
  tempValueToCiv: Map[ps.TempValue, CollectableIrValue] = Map(),
  pointerIdentities: Map[ps.TempValue, GcPointerIdentity]  = Map()
) extends ((ps.TempValue) => CollectableIrValue) {
  /** Returns the IR value for a temp value */
  def apply(tempValue: ps.TempValue): CollectableIrValue =
    tempValueToCiv(tempValue)

  def get(tempValue: ps.TempValue): Option[CollectableIrValue] =
    tempValueToCiv.get(tempValue)

  def keySet = tempValueToCiv.keySet

  def +(civTuple: (ps.TempValue, CollectableIrValue)) = {
    val (tempValue, civ) = civTuple

    val newPointerIdentities = if (civ.gcRoot) {
      pointerIdentities + (tempValue -> new GcPointerIdentity)
    }
    else {
      pointerIdentities
    }

    this.copy(
      tempValueToCiv=(tempValueToCiv + civTuple),
      pointerIdentities=newPointerIdentities
    )
  }

  def ++(civTuples: GenTraversableOnce[(ps.TempValue, CollectableIrValue)]) = {
    civTuples.foldLeft(this) { case (state, civTuple) =>
      state + civTuple
    }
  }

  def -(tempValue: ps.TempValue) = {
    this.copy(
      tempValueToCiv=tempValueToCiv - tempValue,
      pointerIdentities=pointerIdentities - tempValue
    )
  }

  def --(tempValues: GenTraversableOnce[ps.TempValue]) = {
    this.copy(
      tempValueToCiv=tempValueToCiv -- tempValues,
      pointerIdentities=pointerIdentities -- tempValues
    )
  }

  def withUpdatedValues(updates: GenTraversableOnce[(ps.TempValue, CollectableIrValue)]) =
    this.copy(
      tempValueToCiv=tempValueToCiv ++ updates
    )

  def withAliasedTempValue(originalTemp: ps.TempValue, tempTuple: (ps.TempValue, IrValue)) = {
    val gcRoot = tempValueToCiv(originalTemp).gcRoot
    val civTuple = tempTuple._1 -> CollectableIrValue(tempTuple._2, gcRoot)

    if (gcRoot) {
      val aliasedTemp = tempTuple._1
      // Use the pointer identity from the original value
      val pointerIdentity = pointerIdentities(originalTemp)

      this.copy(
        tempValueToCiv=tempValueToCiv + civTuple,
        pointerIdentities=(pointerIdentities + (aliasedTemp -> pointerIdentity))
      )
    }
    else {
      this.copy(
        tempValueToCiv=tempValueToCiv + civTuple
      )
    }
  }
}
