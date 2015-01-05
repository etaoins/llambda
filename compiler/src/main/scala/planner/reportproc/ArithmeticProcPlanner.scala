package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.DivideByZeroException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ArithmeticProcPlanner extends ReportProcPlanner {
  private type BinaryInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private type StaticIntegerOp = (Long, Long) => Long
  private type StaticDoubleOp = (Double, Double) => Double

  private def numberToDoubleTemp(
      value : iv.IntermediateValue,
      isFlonum : Boolean
  )(implicit plan : PlanWriter) : ps.TempValue =
    if (isFlonum) {
      value.toTempValue(vt.Double)
    }
    else {
      val intTemp = value.toTempValue(vt.Int64)
      val convertTemp = ps.Temp(vt.Double)
      plan.steps += ps.ConvertNativeIntegerToFloat(convertTemp, intTemp, true, vt.Double)

      convertTemp
    }

  private def performBinaryMixedOp(
      intInstr : BinaryInstrBuilder,
      flonumInstr : BinaryInstrBuilder,
      staticIntCalc : StaticIntegerOp,
      staticFlonumCalc : StaticDoubleOp,
      args : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = args.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (iv.ConstantExactIntegerValue(constantIntVal1), iv.ConstantExactIntegerValue(constantIntVal2)) =>
        iv.ConstantExactIntegerValue(staticIntCalc(constantIntVal1, constantIntVal2))

      case (iv.ConstantFlonumValue(constantFlonumVal1), iv.ConstantFlonumValue(constantFlonumVal2)) =>
        iv.ConstantFlonumValue(staticFlonumCalc(constantFlonumVal1, constantFlonumVal2))

      case (iv.ConstantExactIntegerValue(constantIntVal1), iv.ConstantFlonumValue(constantFlonumVal2)) =>
        iv.ConstantFlonumValue(staticFlonumCalc(constantIntVal1.toDouble, constantFlonumVal2))

      case (iv.ConstantFlonumValue(constantFlonumVal1), iv.ConstantExactIntegerValue(constantIntVal2)) =>
        iv.ConstantFlonumValue(staticFlonumCalc(constantFlonumVal1, constantIntVal2.toDouble))

      case (dynamic1, dynamic2) =>
        val dynamic1IsInt = dynamic1.hasDefiniteType(vt.ExactIntegerType)
        val dynamic1IsFlonum = dynamic1.hasDefiniteType(vt.FlonumType)

        val dynamic2IsInt = dynamic2.hasDefiniteType(vt.ExactIntegerType)
        val dynamic2IsFlonum = dynamic2.hasDefiniteType(vt.FlonumType)

        if (!(dynamic1IsInt || dynamic1IsFlonum) || !(dynamic2IsInt || dynamic2IsFlonum)) {
          // We don't have definite types; abort
          return None
        }
        else if (dynamic1IsInt && dynamic2IsInt) {
          // Both integers
          val intTemp1 = dynamic1.toTempValue(vt.Int64)(inlinePlan)
          val intTemp2 = dynamic2.toTempValue(vt.Int64)(inlinePlan)
          val resultTemp = ps.Temp(vt.Int64)

          inlinePlan.steps += intInstr(resultTemp, intTemp1, intTemp2)

          new iv.NativeExactIntegerValue(resultTemp, vt.Int64)
        }
        else {
          // At least one is a double
          val doubleTemp1 = numberToDoubleTemp(dynamic1, dynamic1IsFlonum)(inlinePlan)
          val doubleTemp2 = numberToDoubleTemp(dynamic2, dynamic2IsFlonum)(inlinePlan)

          val resultTemp = ps.Temp(vt.Double)
          inlinePlan.steps += flonumInstr(resultTemp, doubleTemp1, doubleTemp2)

          new iv.NativeFlonumValue(resultTemp, vt.Double)
        }

    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue)
  }

  private def performNumericDivide(
      args : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = args.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (iv.ConstantExactIntegerValue(numerInt),  iv.ConstantExactIntegerValue(denomInt)) =>
        if ((denomInt != 0) && ((numerInt % denomInt) == 0)) {
          // This divides exactly
          iv.ConstantExactIntegerValue(numerInt / denomInt)
        }
        else {
          // This does not divide exactly
          iv.ConstantFlonumValue(numerInt.toDouble / denomInt.toDouble)
        }

      case (numerNumValue : iv.ConstantNumberValue, denomNumValue : iv.ConstantNumberValue) =>
        iv.ConstantFlonumValue(numerNumValue.doubleValue / denomNumValue.doubleValue)

      case (dynamicNumer, dynamicDenom) =>
        val dynamicNumerIsInt = dynamicNumer.hasDefiniteType(vt.ExactIntegerType)
        val dynamicNumerIsFlonum = dynamicNumer.hasDefiniteType(vt.FlonumType)

        val dynamicDenomIsInt = dynamicDenom.hasDefiniteType(vt.ExactIntegerType)
        val dynamicDenomIsFlonum = dynamicDenom.hasDefiniteType(vt.FlonumType)

        if (!(dynamicNumerIsInt || dynamicNumerIsFlonum) || !(dynamicDenomIsInt || dynamicDenomIsFlonum)) {
          // We don't have definite types; abort
          return None
        }
        else if (dynamicNumerIsInt && dynamicDenomIsInt) {
          // This result may be exact or inexact - abort and let the library handle it
          return None
        }
        else  {
          val doubleTemp1 = numberToDoubleTemp(dynamicNumer, dynamicNumerIsFlonum)(inlinePlan)
          val doubleTemp2 = numberToDoubleTemp(dynamicDenom, dynamicDenomIsFlonum)(inlinePlan)

          val resultTemp = ps.Temp(vt.Double)
          inlinePlan.steps += ps.FloatDiv(resultTemp, doubleTemp1, doubleTemp2)

          new iv.NativeFlonumValue(resultTemp, vt.Double)
        }
    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue)
  }

  private def performIntegerDivOp(
      instr : BinaryInstrBuilder,
      staticCalc : StaticIntegerOp,
      numerator : (ContextLocated, iv.IntermediateValue),
      denominator : (ContextLocated, iv.IntermediateValue)
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    (numerator, denominator) match {
      case (_, (denomLoc, iv.ConstantExactIntegerValue(0))) =>
        // Catch divide by zero first
        throw new DivideByZeroException(denomLoc, "Attempted integer division by zero")

      case ((_, iv.ConstantExactIntegerValue(constantNumerVal)),
            (_, iv.ConstantExactIntegerValue(constantDenomVal))) =>
        val resultValue = staticCalc(constantNumerVal, constantDenomVal)
        Some(iv.ConstantExactIntegerValue(resultValue))

      case ((numerLoc, dynamicNumer),
            (_, constantDenom : iv.ConstantExactIntegerValue)) =>
        val numerTemp = plan.withContextLocation(numerLoc) {
          dynamicNumer.toTempValue(vt.Int64)
        }

        val denomTemp = constantDenom.toTempValue(vt.Int64)

        val resultTemp = ps.Temp(vt.Int64)
        plan.steps += instr(resultTemp, numerTemp, denomTemp)

        Some(new iv.NativeExactIntegerValue(resultTemp, vt.Int64))

      case _ =>
        None
    }
  }

  private def performIntegerDivide(
      numerator : (ContextLocated, iv.IntermediateValue),
      denominator : (ContextLocated, iv.IntermediateValue)
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] =
    performIntegerDivOp(ps.IntegerDiv(_, true, _, _), _ / _, numerator, denominator)

  private def performIntegerRemainder(
      numerator : (ContextLocated, iv.IntermediateValue),
      denominator : (ContextLocated, iv.IntermediateValue)
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] =
    performIntegerDivOp(ps.IntegerRem(_, true, _, _), _ % _, numerator, denominator)

  override def planWithValues(state : PlannerState)(
    reportName : String,
    args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = (reportName, args) match {
    case ("truncate/", List(numerator, denominator)) =>
      // Handle this here because it produces multiple values
      val inlinePlan = plan.forkPlan()

      val quotientValueOpt = performIntegerDivide(numerator, denominator)
      val remainderValueOpt = performIntegerRemainder(numerator, denominator)

      (quotientValueOpt, remainderValueOpt) match {
        case (Some(quotientValue), Some(remainderValue)) =>
          plan.steps ++= inlinePlan.steps
          Some(ResultValues(List(quotientValue, remainderValue)))

        case _ =>
          None
      }

    case _ =>
      planWithSingleValue(state)(reportName, args) map { value =>
        SingleValue(value)
      }
  }

  private def planWithSingleValue(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("+", Nil) =>
      Some(iv.ConstantExactIntegerValue(0))

    case ("*", Nil) =>
      Some(iv.ConstantExactIntegerValue(1))

    case (reportName, List((argSourceLoc, singleArg))) if List("+", "*").contains(reportName) =>
      // Make sure the arg is numeric
      val numericValue = plan.withContextLocation(argSourceLoc) {
        singleArg.castToSchemeType(vt.NumberType)
      }

      // Return it directly
      Some(numericValue)

    case ("+", multipleArgs) =>
      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerAdd.apply,
        flonumInstr=ps.FloatAdd.apply,
        staticIntCalc=_ + _,
        staticFlonumCalc=_ + _,
        args=argValues
      )

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("-", List((_, singleArg))) =>
      // This is a special case that negates the passed value
      val constantZero = iv.ConstantExactIntegerValue(0)
      performBinaryMixedOp(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        args=List(constantZero, singleArg)
      )

    case ("-", multipleArgs) =>
      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        args=argValues
      )

    case ("*", multipleArgs) =>
      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerMul.apply,
        flonumInstr=ps.FloatMul.apply,
        staticIntCalc=_ * _,
        staticFlonumCalc=_ * _,
        args=argValues
      )

    case ("/", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("/", List((_, singleArg))) =>
      // This is a special case that negates the passed value
      val constantZero = iv.ConstantExactIntegerValue(1)
      performNumericDivide(args=List(constantZero, singleArg))

    case ("/", multipleArgs) =>
      val argValues = multipleArgs.map(_._2)
      performNumericDivide(argValues)

    case ("truncate-quotient", List(numerator, denominator)) =>
      performIntegerDivide(numerator, denominator)

    case ("truncate-remainder", List(numerator, denominator)) =>
      performIntegerRemainder(numerator, denominator)

    case ("expt", List((_, iv.ConstantExactIntegerValue(2)), (_, iv.ConstantExactIntegerValue(power))))
        if (power >= 0) && (power <= 62) =>
      Some(iv.ConstantExactIntegerValue(1L << power))

    case _ =>
      None
  }
}
