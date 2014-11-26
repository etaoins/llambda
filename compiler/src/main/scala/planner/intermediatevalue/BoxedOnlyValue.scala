package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

/** Trait for intermediate values that have no native representation */
trait BoxedOnlyValue extends IntermediateValue {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    impossibleConversion(s"Cannot convert ${typeDescription} to requested type ${vt.NameForType(nativeType)} or any other native type")
}

