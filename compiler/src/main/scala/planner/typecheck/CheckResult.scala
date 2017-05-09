package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

/** Result of a type check
  *
  * This is similar to an iv.IntermediateValue restricted to naitve booleans with more convenient accessors. Notable
  * the conversion methods don't require a world pointer as none of the conversions produce GC allocated values
  */
sealed trait CheckResult {
  /** Converts this type check result to an IntermediateValue */
  def toIntermediateValue: iv.IntermediateValue

  /** Converts this type check result in to a 1 bit LLVM predicate */
  def toNativePred()(implicit plan: PlanWriter): ps.TempValue
}

/** Represents a type check result that is caculated at runtime */
case class DynamicResult(predTemp: ps.TempValue) extends CheckResult {
  def toIntermediateValue =
    new iv.NativePredicateValue(predTemp)

  def toNativePred()(implicit plan: PlanWriter) =
    predTemp
}

/** Represents a type check result that's known at compile time */
case class StaticResult(result: Boolean) extends CheckResult {
  def toIntermediateValue =
    iv.ConstantBooleanValue(result)

  def toNativePred()(implicit plan: PlanWriter) = {
    val resultTemp = ps.TempValue()
    plan.steps += ps.CreateNativeInteger(resultTemp, if (result) 1 else 0, vt.Predicate.bits)
    resultTemp
  }
}

/** Represents a known true type check result */
object StaticTrueResult extends StaticResult(true)
/** Represents a known false type check result */
object StaticFalseResult extends StaticResult(false)
