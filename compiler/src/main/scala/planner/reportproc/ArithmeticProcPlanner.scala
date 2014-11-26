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
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (constantInt1 : iv.ConstantExactIntegerValue, constantInt2 : iv.ConstantExactIntegerValue) =>
        new iv.ConstantExactIntegerValue(staticIntCalc(constantInt1.value, constantInt2.value))

      case (constantFlonum1 : iv.ConstantFlonumValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        new iv.ConstantFlonumValue(staticFlonumCalc(constantFlonum1.value, constantFlonum2.value))

      case (constantInt1 : iv.ConstantExactIntegerValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        val flonum1 = constantInt1.value.toDouble
        new iv.ConstantFlonumValue(staticFlonumCalc(flonum1, constantFlonum2.value))

      case (constantFlonum1 : iv.ConstantFlonumValue, constantInt2 : iv.ConstantExactIntegerValue) =>
        val flonum2 = constantInt2.value.toDouble
        new iv.ConstantFlonumValue(staticFlonumCalc(constantFlonum1.value, flonum2))

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
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (numerIntValue : iv.ConstantExactIntegerValue, denomIntValue : iv.ConstantExactIntegerValue) =>
        val numerInt = numerIntValue.value
        val denomInt = denomIntValue.value

        if ((denomInt != 0) && ((numerInt % denomInt) == 0))
        {
          // This divides exactly
          new iv.ConstantExactIntegerValue(numerInt / denomInt)
        }
        else
        {
          // This does not divide exactly
          new iv.ConstantFlonumValue(numerInt.toDouble / denomInt.toDouble)
        }

      case (numerNumValue : iv.ConstantNumberValue, denomNumValue : iv.ConstantNumberValue) =>
        new iv.ConstantFlonumValue(numerNumValue.doubleValue / denomNumValue.doubleValue)

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
      case (_, (denomLoc, constantDenom : iv.ConstantExactIntegerValue)) if constantDenom.value == 0 =>
        // Catch divide by zero first
        throw new DivideByZeroException(denomLoc, "Attempted integer division by zero")

      case ((_, constantNumer : iv.ConstantExactIntegerValue),
            (_, constantDenom : iv.ConstantExactIntegerValue)) =>
        val resultValue = staticCalc(constantNumer.value, constantDenom.value)
        Some(new iv.ConstantExactIntegerValue(resultValue))

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
    operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[ResultValues] = (reportName, operands) match {
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
      planWithSingleValue(state)(reportName, operands) map { value =>
        SingleValue(value)
      }
  }

  private def planWithSingleValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("+", Nil) =>
      Some(new iv.ConstantExactIntegerValue(0))

    case ("*", Nil) =>
      Some(new iv.ConstantExactIntegerValue(1))

    case (reportName, List((operandSourceLoc, singleOperand))) if List("+", "*").contains(reportName) =>
      // Make sure the operand is numeric
      val numericValue = plan.withContextLocation(operandSourceLoc) {
        singleOperand.castToSchemeType(vt.NumberType)
      }

      // Return it directly
      Some(numericValue)

    case ("+", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerAdd.apply,
        flonumInstr=ps.FloatAdd.apply,
        staticIntCalc=_ + _,
        staticFlonumCalc=_ + _,
        operands=operandValues
      )

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("-", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(0)
      performBinaryMixedOp(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=List(constantZero, singleOperand)
      )

    case ("-", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=operandValues
      )

    case ("*", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.IntegerMul.apply,
        flonumInstr=ps.FloatMul.apply,
        staticIntCalc=_ * _,
        staticFlonumCalc=_ * _,
        operands=operandValues
      )

    case ("/", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("/", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(1)
      performNumericDivide(operands=List(constantZero, singleOperand))

    case ("/", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2)
      performNumericDivide(operandValues)

    case ("truncate-quotient", List(numerator, denominator)) =>
      performIntegerDivide(numerator, denominator)

    case ("truncate-remainder", List(numerator, denominator)) =>
      performIntegerRemainder(numerator, denominator)

    case _ =>
      None
  }
}
