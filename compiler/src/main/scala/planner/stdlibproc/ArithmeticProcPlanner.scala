package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ContextLocated, ErrorCategory}
import llambda.compiler.{DivideByZeroException, IntegerOverflowException}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._


object ArithmeticProcPlanner extends StdlibProcPlanner {
  private type UncheckedInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private type CheckedInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue, RuntimeErrorMessage) => ps.Step
  private type StaticIntegerOp = (BigInt, BigInt) => BigInt
  private type StaticDoubleOp = (Double, Double) => Double

  private def performBinaryMixedOp(
      intInstr: CheckedInstrBuilder,
      flonumInstr: UncheckedInstrBuilder,
      staticIntCalc: StaticIntegerOp,
      staticFlonumCalc: StaticDoubleOp,
      operands: List[iv.IntermediateValue],
      intOverflowMessage: RuntimeErrorMessage
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val typedOperands = operands.map(TypedNumberValue.fromIntermediateValue)

    if (typedOperands.exists(_.isInstanceOf[TypedNumberValue.Unknown])) {
      // We don't have enough type information to continue
      return None
    }

    // Determine if our result is a flonum
    val resultIsFlonum = typedOperands.exists(_.isInstanceOf[TypedNumberValue.Flonum])

    val resultValue = typedOperands.reduceLeft { (op1: TypedNumberValue, op2: TypedNumberValue) => (op1, op2) match {
      case (TypedNumberValue.ConstantInteger(constantIntVal1), TypedNumberValue.ConstantInteger(constantIntVal2)) =>
        val bigIntResult = staticIntCalc(BigInt(constantIntVal1), BigInt(constantIntVal2))

        if (bigIntResult.isValidLong) {
          TypedNumberValue.ConstantInteger(bigIntResult.toLong)
        }
        else if (resultIsFlonum) {
          // Promote this to a flonum
          TypedNumberValue.ConstantFlonum(bigIntResult.toDouble)
        }
        else {
          // Integer result was expected!
          throw new IntegerOverflowException(plan.activeContextLocated, intOverflowMessage.text)
        }

      case (TypedNumberValue.ConstantFlonum(constantFlonumVal1), TypedNumberValue.ConstantFlonum(constantFlonumVal2)) =>
        TypedNumberValue.ConstantFlonum(staticFlonumCalc(constantFlonumVal1, constantFlonumVal2))

      case (TypedNumberValue.ConstantInteger(constantIntVal1), TypedNumberValue.ConstantFlonum(constantFlonumVal2)) =>
        TypedNumberValue.ConstantFlonum(staticFlonumCalc(constantIntVal1.toDouble, constantFlonumVal2))

      case (TypedNumberValue.ConstantFlonum(constantFlonumVal1), TypedNumberValue.ConstantInteger(constantIntVal2)) =>
        TypedNumberValue.ConstantFlonum(staticFlonumCalc(constantFlonumVal1, constantIntVal2.toDouble))

      case (TypedNumberValue.Integer(other1), TypedNumberValue.Integer(other2)) =>
        if (resultIsFlonum) {
          // We expect a flonum result - this requires re-trying this computation as flonum on overflow
          // This is tricky and rare so just let the runtime handle it
          return None
        }

        // Both integers
        val intTemp1 = other1.toTempValue(vt.Int64)(inlinePlan)
        val intTemp2 = other2.toTempValue(vt.Int64)(inlinePlan)

        val resultTemp = ps.TempValue()

        inlinePlan.steps += intInstr(resultTemp, intTemp1, intTemp2, intOverflowMessage)

        TypedNumberValue.Integer(new iv.NativeIntegerValue(resultTemp, vt.Int64)())

      case (known1: TypedNumberValue.Known, known2: TypedNumberValue.Known) =>
        // At least one is a double
        val doubleTemp1 = known1.toDoubleTemp()(inlinePlan)
        val doubleTemp2 = known2.toDoubleTemp()(inlinePlan)

        val resultTemp = ps.TempValue()
        inlinePlan.steps += flonumInstr(resultTemp, doubleTemp1, doubleTemp2)

        TypedNumberValue.Flonum(new iv.NativeFlonumValue(resultTemp, vt.Double))
    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue.value)
  }

  private def performNumericDivide(
      operands: List[iv.IntermediateValue]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1: iv.IntermediateValue, op2: iv.IntermediateValue) => (op1, op2) match {
      case (iv.ConstantIntegerValue(Long.MinValue), iv.ConstantIntegerValue(-1)) =>
        // This would cause an integer overflow
        iv.ConstantFlonumValue(Long.MinValue.toDouble / -1.toDouble)

      case (iv.ConstantIntegerValue(numerInt), iv.ConstantIntegerValue(denomInt)) =>
        if (denomInt == 0) {
          throw new DivideByZeroException(plan.activeContextLocated, "Attempted (/) by integer zero")
        }
        else if ((numerInt % denomInt) == 0) {
          // This divides exactly
          iv.ConstantIntegerValue(numerInt / denomInt)
        }
        else {
          // This does not divide exactly
          iv.ConstantFlonumValue(numerInt.toDouble / denomInt.toDouble)
        }

      case (numerNumValue: iv.ConstantNumberValue, iv.ConstantIntegerValue(denomIntVal)) =>
        if (denomIntVal == 0) {
          throw new DivideByZeroException(plan.activeContextLocated, "Attempted (/) by integer zero")
        }

        iv.ConstantFlonumValue(numerNumValue.doubleValue / denomIntVal.toDouble)

      case (numerNumValue: iv.ConstantNumberValue, iv.ConstantFlonumValue(denomFlonumVal)) =>
        iv.ConstantFlonumValue(numerNumValue.doubleValue / denomFlonumVal)

      case (dynamicNumer, dynamicDenom) =>
        val typedNumer = TypedNumberValue.fromIntermediateValue(dynamicNumer)
        val typedDenom = TypedNumberValue.fromIntermediateValue(dynamicDenom)

        (typedNumer, typedDenom) match {
          case (_, TypedNumberValue.Integer(_)) =>
            // This could be an integer divide by zero. Even if the denominator is non-zero if the numerator is also
            // an integer we don't know the flonumness of our result. Let the library handle this.
            return None

          case (knownNumer: TypedNumberValue.Known, knownDenom: TypedNumberValue.Known) =>
            val doubleNumerTemp = knownNumer.toDoubleTemp()(inlinePlan)
            val doubleDenomTemp = knownDenom.toDoubleTemp()(inlinePlan)

            val resultTemp = ps.TempValue()
            inlinePlan.steps += ps.FloatDiv(resultTemp, doubleNumerTemp, doubleDenomTemp)

            new iv.NativeFlonumValue(resultTemp, vt.Double)

          case _ =>
            // We don't have definite types; abort
            return None
        }
      }
    }

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue)
  }

  private def performIntegerDivOp(
      instr: UncheckedInstrBuilder,
      staticCalc: StaticIntegerOp,
      numerator: (ContextLocated, iv.IntermediateValue),
      denominator: (ContextLocated, iv.IntermediateValue)
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (numerator, denominator) match {
    case (_, (denomLoc, iv.ConstantIntegerValue(0))) =>
      // Catch divide by zero first
      throw new DivideByZeroException(denomLoc, "Attempted integer division by zero")

    case ((_, iv.ConstantIntegerValue(Long.MinValue)),
          (denomLoc, iv.ConstantIntegerValue(-1))) =>
      throw new IntegerOverflowException(denomLoc, "Integer overflow during division")

    case ((_, iv.ConstantIntegerValue(constantNumerVal)),
          (_, iv.ConstantIntegerValue(constantDenomVal))) =>
      val resultValue = staticCalc(constantNumerVal, constantDenomVal)
      Some(iv.ConstantIntegerValue(resultValue.toLong))

    case ((numerLoc, dynamicNumer), (_, knownDenom: iv.KnownInteger)) =>
      val possibleDenomValues = knownDenom.possibleValues

      if (possibleDenomValues.contains(0) || possibleDenomValues.contains(-1)) {
        // This could cause a divide-by-zero or overflow at runtime. Punt to the runtime implementation.
        None
      }
      else {
        val numerTemp = plan.withContextLocation(numerLoc) {
          dynamicNumer.toTempValue(vt.Int64)
        }

        val denomTemp = knownDenom.toTempValue(vt.Int64)

        val resultTemp = ps.TempValue()
        plan.steps += instr(resultTemp, numerTemp, denomTemp)

        Some(new iv.NativeIntegerValue(resultTemp, vt.Int64)())
      }

    case _ =>
      None
  }

  private def performIntegerDivide(
      numerator: (ContextLocated, iv.IntermediateValue),
      denominator: (ContextLocated, iv.IntermediateValue)
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] =
    performIntegerDivOp(ps.IntegerDiv(_, _, _), _ / _, numerator, denominator)

  private def performIntegerRemainder(
      numerator: (ContextLocated, iv.IntermediateValue),
      denominator: (ContextLocated, iv.IntermediateValue)
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = denominator._2 match {
    case iv.ConstantIntegerValue(1) | iv.ConstantIntegerValue(-1) =>
      // This is both an optimisation and required to avoid overflow when dividing Long.MinValue by -1
      Some(iv.ConstantIntegerValue(0))

    case _ =>
      performIntegerDivOp(ps.IntegerRem(_, _, _), _ % _, numerator, denominator)
  }

  override def planWithValue(state: PlannerState)(
    reportName: String,
    args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("truncate/", List(numerator, denominator)) =>
      val inlinePlan = plan.forkPlan()

      val quotientValueOpt = performIntegerDivide(numerator, denominator)
      val remainderValueOpt = performIntegerRemainder(numerator, denominator)

      (quotientValueOpt, remainderValueOpt) match {
        case (Some(quotientValue), Some(remainderValue)) =>
          plan.steps ++= inlinePlan.steps
          Some(ValuesToPair(quotientValue, remainderValue))

        case _ =>
          None
      }

    case ("+", Nil) =>
      Some(iv.ConstantIntegerValue(0))

    case ("*", Nil) =>
      Some(iv.ConstantIntegerValue(1))

    case ("+" | "*", List((argSourceLoc, singleArg))) =>
      // Make sure the arg is numeric
      val numericValue = plan.withContextLocation(argSourceLoc) {
        singleArg.castToSchemeType(vt.NumberType)
      }

      // Return it directly
      Some(numericValue)

    case ("+", multipleArgs) =>
      val intOverflowMessage = RuntimeErrorMessage(
        category=ErrorCategory.IntegerOverflow,
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
        text="Integer overflow in inverting (-)"
      )

      // This is a special case that negates the passed value
      val constantZero = iv.ConstantIntegerValue(0)
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
      // This is a special case that calculates the reciprocal of the passed value
      val constantOne = iv.ConstantIntegerValue(1)
      performNumericDivide(List(constantOne, singleArg))

    case ("/", multipleArgs) =>
      val argValues = multipleArgs.map(_._2)
      performNumericDivide(argValues)

    case ("truncate-quotient", List(numerator, denominator)) =>
      performIntegerDivide(numerator, denominator)

    case ("truncate-remainder", List(numerator, denominator)) =>
      performIntegerRemainder(numerator, denominator)

    case ("expt", List((_, iv.ConstantIntegerValue(2)), (_, iv.ConstantIntegerValue(power))))
        if (power >= 0) && (power <= 62) =>
      Some(iv.ConstantIntegerValue(1L << power))

    case ("floor" | "ceiling" | "truncate" | "round", List((_, constInt: iv.ConstantIntegerValue))) =>
      Some(constInt)

    case ("floor", List((_, iv.ConstantFlonumValue(value)))) =>
      Some(new iv.ConstantFlonumValue(Math.floor(value)))

    case ("ceiling", List((_, iv.ConstantFlonumValue(value)))) =>
      Some(new iv.ConstantFlonumValue(Math.ceil(value)))

    case ("round", List((_, iv.ConstantFlonumValue(value)))) =>
      Some(new iv.ConstantFlonumValue(Math.round(value)))

    case _ =>
      None
  }
}
