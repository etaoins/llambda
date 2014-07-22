package io.llambda.compiler.planner.typecheck
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}

/** Result of a type check
  *
  * This is similar to an iv.IntermediateValue restricted to naitve booleans with more convenient accessors. Notable
  * the conversion methods don't require a world pointer as none of the conversions produce GC managed values
  */
sealed trait CheckResult {
  /** Converts this type check result to an IntermediateValue */
  def toIntermediateValue : iv.IntermediateValue

  /** Converts this type check result in to an 8 bit C-style boolean */
  def toNativeCBool()(implicit plan : PlanWriter) : ps.TempValue
  /** Converts this type check result in to a 1 bit LLVM predicate */
  def toNativePred()(implicit plan : PlanWriter) : ps.TempValue
}

/** Represents a type check result that is caculated at runtime */
sealed trait DynamicResult extends CheckResult {
  protected def castToNativeValue(tempValue : ps.TempValue, nativeType : vt.IntLikeType)(plan : PlanWriter)  : ps.TempValue = {
    val resultTemp = ps.Temp(nativeType)
    plan.steps += ps.ConvertNativeInteger(resultTemp, tempValue, nativeType.bits, false) 
    resultTemp
  }
}

private[typecheck] case class DynamicPredResult(predTemp : ps.TempValue) extends DynamicResult {
  def toIntermediateValue = 
    new iv.NativePredicateValue(predTemp)

  def toNativeCBool()(implicit plan : PlanWriter) =
    castToNativeValue(predTemp, vt.CBool)(plan)

  def toNativePred()(implicit plan : PlanWriter) =
    predTemp
}

private[typecheck] case class DynamicCBoolResult(boolTemp : ps.TempValue) extends DynamicResult {
  def toIntermediateValue = 
    new iv.NativeBooleanValue(boolTemp)

  def toNativeCBool()(implicit plan : PlanWriter) =
    boolTemp

  def toNativePred()(implicit plan : PlanWriter) =
    castToNativeValue(boolTemp, vt.Predicate)(plan)
}

/** Represents a type check result that's known at compile time */
case class StaticResult(result : Boolean) extends CheckResult {
  def toIntermediateValue = 
    new iv.ConstantBooleanValue(result)

  private def toNativeValue(nativeType : vt.IntLikeType)(implicit plan : PlanWriter) : ps.TempValue = {
    val resultTemp = ps.Temp(nativeType)
    plan.steps += ps.CreateNativeInteger(resultTemp, if (result) 1 else 0, nativeType.bits)
    resultTemp
  }

  def toNativeCBool()(implicit plan : PlanWriter) =
    toNativeValue(vt.CBool)

  def toNativePred()(implicit plan : PlanWriter) =
    toNativeValue(vt.Predicate)
}

/** Represents a known true type check result */
object StaticTrueResult extends StaticResult(true)
/** Represents a known false type check result */
object StaticFalseResult extends StaticResult(false)
