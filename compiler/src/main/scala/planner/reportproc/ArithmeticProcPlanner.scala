package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ContextLocated, ErrorCategory}
import llambda.compiler.{DivideByZeroException, IntegerOverflowException}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ArithmeticProcPlanner extends ReportProcPlanner {
  private type UncheckedInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private type CheckedInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue, RuntimeErrorMessage) => ps.Step
  private type StaticIntegerOp = (BigInt, BigInt) => BigInt
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

  sealed abstract class KnownOperandType
  case object KnownInt extends KnownOperandType
  case object KnownFlonum extends KnownOperandType

  case class TypedOperand(value : iv.IntermediateValue, knownType : KnownOperandType)

  private def performBinaryMixedOp(
      intInstr : CheckedInstrBuilder,
      flonumInstr : UncheckedInstrBuilder,
      staticIntCalc : StaticIntegerOp,
      staticFlonumCalc : StaticDoubleOp,
      operands : List[iv.IntermediateValue],
      intOverflowMessage : RuntimeErrorMessage
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val typedOperands = operands map { operand =>
      if (operand.hasDefiniteType(vt.ExactIntegerType)) {
        TypedOperand(operand, KnownInt)
      }
      else if (operand.hasDefiniteType(vt.FlonumType)) {
        TypedOperand(operand, KnownFlonum)
      }
      else {
        // We don't know the precise type - we can't continue
        return None
      }
    }

    // Determine if our result is inexact
    val resultInexact = typedOperands.exists { typedArg =>
      typedArg.knownType == KnownFlonum
    }

    val resultValue = typedOperands.reduceLeft { (op1 : TypedOperand, op2 : TypedOperand) => (op1, op2) match {
      case (TypedOperand(iv.ConstantExactIntegerValue(constantIntVal1), _),
            TypedOperand(iv.ConstantExactIntegerValue(constantIntVal2), _)) =>
        val exactResult = staticIntCalc(BigInt(constantIntVal1), BigInt(constantIntVal2))

        if (exactResult.isValidLong) {
          TypedOperand(iv.ConstantExactIntegerValue(exactResult.toLong), KnownInt)
        }
        else if (resultInexact) {
          // Promote this to a double
          TypedOperand(iv.ConstantFlonumValue(exactResult.toDouble), KnownFlonum)
        }
        else {
          // Exact result was expected!
          throw new IntegerOverflowException(plan.activeContextLocated, intOverflowMessage.text)
        }

      case (TypedOperand(iv.ConstantFlonumValue(constantFlonumVal1), _),
            TypedOperand(iv.ConstantFlonumValue(constantFlonumVal2), _)) =>
        TypedOperand(
          iv.ConstantFlonumValue(staticFlonumCalc(constantFlonumVal1, constantFlonumVal2)),
          KnownFlonum
        )

      case (TypedOperand(iv.ConstantExactIntegerValue(constantIntVal1), _),
            TypedOperand(iv.ConstantFlonumValue(constantFlonumVal2), _)) =>
        TypedOperand(
          iv.ConstantFlonumValue(staticFlonumCalc(constantIntVal1.toDouble, constantFlonumVal2)),
          KnownFlonum
        )

      case (TypedOperand(iv.ConstantFlonumValue(constantFlonumVal1), _),
            TypedOperand(iv.ConstantExactIntegerValue(constantIntVal2), _)) =>
        TypedOperand(
          iv.ConstantFlonumValue(staticFlonumCalc(constantFlonumVal1, constantIntVal2.toDouble)),
          KnownFlonum
        )

      case (TypedOperand(other1, type1), TypedOperand(other2, type2)) =>
        if ((type1 == KnownInt) && (type2 == KnownInt)) {
          if (resultInexact) {
            // We expect an inexact result - this requires re-trying this computation as inexact on overflow
            // This is tricky and rare so just let the runtime handle it
            return None
          }

          // Both integers
          val intTemp1 = other1.toTempValue(vt.Int64)(inlinePlan)
          val intTemp2 = other2.toTempValue(vt.Int64)(inlinePlan)

          val resultTemp = ps.Temp(vt.Int64)
          val overflowTemp = ps.Temp(vt.Predicate)

          inlinePlan.steps += intInstr(resultTemp, intTemp1, intTemp2, intOverflowMessage)

          TypedOperand(new iv.NativeExactIntegerValue(resultTemp, vt.Int64), KnownInt)
        }
        else {
          // At least one is a double
          val doubleTemp1 = numberToDoubleTemp(other1, type1 == KnownFlonum)(inlinePlan)
          val doubleTemp2 = numberToDoubleTemp(other2, type2 == KnownFlonum)(inlinePlan)

          val resultTemp = ps.Temp(vt.Double)
          inlinePlan.steps += flonumInstr(resultTemp, doubleTemp1, doubleTemp2)

          TypedOperand(new iv.NativeFlonumValue(resultTemp, vt.Double), KnownFlonum)
        }
    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue.value)
  }

  private def performNumericDivide(
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (iv.ConstantExactIntegerValue(Long.MinValue), iv.ConstantExactIntegerValue(-1)) =>
        // This would cause an integer overflow
        iv.ConstantFlonumValue(Long.MinValue.toDouble / -1.toDouble)

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
      instr : UncheckedInstrBuilder,
      staticCalc : StaticIntegerOp,
      numerator : (ContextLocated, iv.IntermediateValue),
      denominator : (ContextLocated, iv.IntermediateValue)
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    (numerator, denominator) match {
      case (_, (denomLoc, iv.ConstantExactIntegerValue(0))) =>
        // Catch divide by zero first
        throw new DivideByZeroException(denomLoc, "Attempted integer division by zero")

      case ((_, iv.ConstantExactIntegerValue(Long.MinValue)),
            (denomLoc, iv.ConstantExactIntegerValue(-1))) =>
        throw new IntegerOverflowException(denomLoc, "Integer overflow during division")

      case ((_, iv.ConstantExactIntegerValue(constantNumerVal)),
            (_, iv.ConstantExactIntegerValue(constantDenomVal))) =>
        val resultValue = staticCalc(constantNumerVal, constantDenomVal)
        Some(iv.ConstantExactIntegerValue(resultValue.toLong))

      // We need a known denominator so we can ensure it won't cause divide-by-zero or overflow. These will be handled
      // properly by the runtime
      case ((numerLoc, dynamicNumer),
            (_, constantDenom @ iv.ConstantExactIntegerValue(constantDenomVal)))
          if (constantDenomVal != 0) && (constantDenomVal != -1) =>
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
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = denominator._2 match {
    case iv.ConstantExactIntegerValue(1) | iv.ConstantExactIntegerValue(-1) =>
      // This is both an optimisation and required to avoid overflow when dividing Long.MinValue by -1
      Some(iv.ConstantExactIntegerValue(0))

    case _ =>
      performIntegerDivOp(ps.IntegerRem(_, true, _, _), _ % _, numerator, denominator)
  }

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
      val intOverflowMessage = RuntimeErrorMessage(
        category=ErrorCategory.IntegerOverflow,
        name="addOverflow",
        text="Integer overflow in (+)"
      )

      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.CheckedIntegerAdd.apply,
        flonumInstr=ps.FloatAdd.apply,
        staticIntCalc=_ + _,
        staticFlonumCalc=_ + _,
        operands=argValues,
        intOverflowMessage=intOverflowMessage
      )

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("-", List((_, singleArg))) =>
      val intOverflowMessage = RuntimeErrorMessage(
        category=ErrorCategory.IntegerOverflow,
        name="invertingSubOverflow",
        text="Integer overflow in inverting (-)"
      )

      // This is a special case that negates the passed value
      val constantZero = iv.ConstantExactIntegerValue(0)
      performBinaryMixedOp(
        intInstr=ps.CheckedIntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=List(constantZero, singleArg),
        intOverflowMessage=intOverflowMessage
      )

    case ("-", multipleArgs) =>
      val intOverflowMessage = RuntimeErrorMessage(
        category=ErrorCategory.IntegerOverflow,
        name="subtractingSubOverflow",
        text="Integer overflow in subtracting (-)"
      )

      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.CheckedIntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=argValues,
        intOverflowMessage=intOverflowMessage
      )

    case ("*", multipleArgs) =>
      val intOverflowMessage = RuntimeErrorMessage(
        category=ErrorCategory.IntegerOverflow,
        name="mulOverflow",
        text="Integer overflow in (*)"
      )

      val argValues = multipleArgs.map(_._2)
      performBinaryMixedOp(
        intInstr=ps.CheckedIntegerMul.apply,
        flonumInstr=ps.FloatMul.apply,
        staticIntCalc=_ * _,
        staticFlonumCalc=_ * _,
        operands=argValues,
        intOverflowMessage=intOverflowMessage
      )

    case ("/", Nil) =>
      // This isn't allowed - let it fail at runtime
      None

    case ("/", List((_, singleArg))) =>
      // This is a special case that negates the passed value
      val constantZero = iv.ConstantExactIntegerValue(1)
      performNumericDivide(List(constantZero, singleArg))

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
