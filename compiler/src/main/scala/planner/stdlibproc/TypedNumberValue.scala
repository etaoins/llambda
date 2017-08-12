package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.planner._

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}


/** Represents an IntermediateValue with additional numeric typing information */
private[stdlibproc] sealed abstract class TypedNumberValue {
  val value: iv.IntermediateValue
}

/** Helper object for performing operations on mixed-typed numbers
  *
  * Scheme has very specific rules for dealing with mixed integer and flonum operations. This object contains helpers
  * for classifying IntermediateValue instances as either integers, floats or of an unknown type.
  */
private[stdlibproc] object TypedNumberValue {
  /** IntermediateValue with a known specific numeric type */
  sealed trait Known extends TypedNumberValue {
    /** Returns a TempValue of type vt.Double
      *
      * For integers a conversion will be performed to its double representation
      */
    def toDoubleTemp()(implicit plan: PlanWriter): ps.TempValue
  }

  /** IntermediateValue known to be a flonum */
  case class Flonum(value: iv.IntermediateValue) extends Known {
    def toDoubleTemp()(implicit plan: PlanWriter): ps.TempValue =
      value.toTempValue(vt.Double)
  }

  /** IntermediateValue known to be an integer */
  case class Integer(value: iv.IntermediateValue) extends Known {
    def toDoubleTemp()(implicit plan: PlanWriter): ps.TempValue = {
      val intTemp = value.toTempValue(vt.Int64)
      val convertedTemp = ps.TempValue()
      plan.steps += ps.ConvertNativeIntegerToFloat(convertedTemp, intTemp, true, vt.Double)

      convertedTemp
    }
  }

  /** IntermediateValue with an unknown numeric type
    *
    * In practice this is typically a value with the <number> type after argument type checking has been done.
    */
  case class Unknown(value: iv.IntermediateValue) extends TypedNumberValue

  object ConstantNumber {
    def unapply(tnv: TypedNumberValue): Option[iv.ConstantNumberValue] = tnv match {
      case Integer(constantInt: iv.ConstantIntegerValue) =>
        Some(constantInt)

      case Flonum(constantFlonum: iv.ConstantFlonumValue) =>
        Some(constantFlonum)

      case _ =>
        None
    }
  }

  object ConstantInteger {
    def apply(value: Long): Integer =
      Integer(iv.ConstantIntegerValue(value))

    def unapply(tnv: TypedNumberValue): Option[Long] = tnv match {
      case Integer(iv.ConstantIntegerValue(value)) =>
        Some(value)

      case _ =>
        None
    }
  }

  object ConstantFlonum {
    def apply(value: Double): Flonum =
      Flonum(iv.ConstantFlonumValue(value))

    def unapply(tnv: TypedNumberValue): Option[Double] = tnv match {
      case Flonum(iv.ConstantFlonumValue(value)) =>
        Some(value)

      case _ =>
        None
    }
  }

  def fromIntermediateValue(value: iv.IntermediateValue): TypedNumberValue =
    if (value.hasDefiniteType(vt.IntegerType)) {
      Integer(value)
    }
    else if (value.hasDefiniteType(vt.FlonumType)) {
      Flonum(value)
    }
    else {
      Unknown(value)
    }
}
