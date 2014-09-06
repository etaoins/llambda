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

/** Tracks the state of temp values during codegen
  *
  * When not using pointer aliases this behaves mostly like a simple Map(). However, explicitly aliased values can be
  * added using withAliasedTempValue. Aliasing information is used by the GC logic to remove redundant rooting of
  * aliased values
  */
case class LiveTemps(
  tempValueToIr : Map[ps.TempValue, IrValue] = Map(),
  pointerIdentities : Map[ps.TempValue, GcPointerIdentity]  = Map()
) extends ((ps.TempValue) => IrValue) {
  /** Returns the IR value for a temp value */
  def apply(tempValue : ps.TempValue) : IrValue =
    tempValueToIr(tempValue)

  def get(tempValue : ps.TempValue) : Option[IrValue] =
    tempValueToIr.get(tempValue)

  def keySet = tempValueToIr.keySet

  def +(tempTuple : (ps.TempValue, IrValue)) = {
    val tempValue = tempTuple._1

    val newPointerIdentities = if (tempValue.isGcManaged) {
      pointerIdentities + (tempValue -> new GcPointerIdentity)
    }
    else {
      pointerIdentities
    }

    this.copy(
      tempValueToIr=(tempValueToIr + tempTuple),
      pointerIdentities=newPointerIdentities
    )
  }
  
  def ++(tempTuples : GenTraversableOnce[(ps.TempValue, IrValue)]) = {
    tempTuples.foldLeft(this) { case (state, tempTuple) =>
      state + tempTuple
    }
  }
  
  def -(tempValue : ps.TempValue) = {
    this.copy(
      tempValueToIr=tempValueToIr - tempValue,
      pointerIdentities=pointerIdentities - tempValue
    )
  }

  def --(tempValues : GenTraversableOnce[ps.TempValue]) = {
    this.copy(
      tempValueToIr=tempValueToIr -- tempValues,
      pointerIdentities=pointerIdentities -- tempValues
    )
  }

  def withUpdatedIrValues(updates : GenTraversableOnce[(ps.TempValue, IrValue)]) =
    this.copy(
      tempValueToIr=tempValueToIr ++ updates
    )

  def withAliasedTempValue(originalTemp : ps.TempValue, tempTuple : (ps.TempValue, IrValue)) =
    if (originalTemp.isGcManaged) {
      val aliasedTemp = tempTuple._1
      // Use the pointer identity from the original value
      val pointerIdentity = pointerIdentities(originalTemp)

      this.copy(
        tempValueToIr=(tempValueToIr + tempTuple),
        pointerIdentities=(pointerIdentities + (aliasedTemp -> pointerIdentity))
      )
    }
    else {
      this.copy(
        tempValueToIr=(tempValueToIr + tempTuple)
      )
    }
}
