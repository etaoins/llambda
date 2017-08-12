package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.planner._
import llambda.compiler.{ContextLocated, InvalidArgumentException}

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}


object NumberProcPlanner extends StdlibProcPlanner {
  private type IntegerCompartor = (Long, Long) => Boolean

  private type DoubleCompartor = (Double, Double) => Boolean

  private sealed abstract class CompareResult
  private case class StaticCompare(result: Boolean) extends CompareResult
  private case class DynamicCompare(nativePred: ps.TempValue) extends CompareResult
  private case object UnplannableCompare extends CompareResult

  private def integerValue(
      value: iv.IntermediateValue
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = value match {
    case knownInt if knownInt.hasDefiniteType(vt.IntegerType) =>
      // Already an int
      Some(knownInt)

    case iv.ConstantFlonumValue(constFlonumVal) =>
      val longValue = constFlonumVal.toLong

      // Make sure this was lossless
      if (longValue.toDouble == constFlonumVal) {
        Some(iv.ConstantIntegerValue(longValue))
      }
      else {
        throw new InvalidArgumentException(plan.activeContextLocated, "Attempted to convert non-integral flonum to integer")
      }

    case _ =>
      None
  }

  private def flonumValue(
      value: iv.IntermediateValue
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = value match {
    case knownFlonum if knownFlonum.hasDefiniteType(vt.FlonumType) =>
      // Already a flonum
      Some(knownFlonum)

    case iv.ConstantIntegerValue(constIntegerVal) =>
      // Statically convert it to a double
      Some(iv.ConstantFlonumValue(constIntegerVal.toDouble))

    case knownInt if knownInt.hasDefiniteType(vt.IntegerType) =>
      val intTemp = knownInt.toTempValue(vt.Int64)
      val doubleTemp = ps.TempValue()

      plan.steps += ps.ConvertNativeIntegerToFloat(doubleTemp, intTemp, true, vt.Double)

      Some(new iv.NativeFlonumValue(doubleTemp, vt.Double))

    case _ =>
      None
  }

  private def compareArgs(
      compareCond: ps.CompareCond,
      staticIntCalc: IntegerCompartor,
      staticFlonumCalc: DoubleCompartor,
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue
  )(implicit plan: PlanWriter): CompareResult = {
    val List(typedVal1, typedVal2) = List(val1, val2).map(TypedNumberValue.fromIntermediateValue)

    (typedVal1, typedVal2) match {
      case (TypedNumberValue.ConstantInteger(constIntegerVal1), TypedNumberValue.ConstantInteger(constIntegerVal2)) =>
        val compareResult = staticIntCalc(constIntegerVal1, constIntegerVal2)
        StaticCompare(compareResult)

      case (TypedNumberValue.ConstantNumber(constNum1), TypedNumberValue.ConstantNumber(constNum2)) =>
        val compareResult = staticFlonumCalc(constNum1.doubleValue, constNum2.doubleValue)
        StaticCompare(compareResult)

      case (TypedNumberValue.Integer(integer1), TypedNumberValue.Integer(integer2)) =>
        val val1Temp = integer1.toTempValue(vt.Int64)
        val val2Temp = integer2.toTempValue(vt.Int64)

        val predicateTemp = ps.TempValue()

        val signed = if (compareCond == ps.CompareCond.Equal) {
          None
        }
        else {
          Some(true)
        }

        // Do a direct integer compare
        plan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

        DynamicCompare(predicateTemp)

      case (known1: TypedNumberValue.Known, known2: TypedNumberValue.Known) =>
        val val1Temp = known1.toDoubleTemp()
        val val2Temp = known2.toDoubleTemp()

        val predicateTemp = ps.TempValue()

        // Do a direct float compare
        plan.steps += ps.FloatCompare(predicateTemp, compareCond, val1Temp, val2Temp)

        DynamicCompare(predicateTemp)

      case _ =>
        UnplannableCompare
    }
  }

  private def compareArgList(
      compareCond: ps.CompareCond,
      staticIntCalc: IntegerCompartor,
      staticFlonumCalc: DoubleCompartor,
      args: List[iv.IntermediateValue]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    // Compare in a fork in case we abort the whole thing later
    val comparePlan = plan.forkPlan()

    val pairwiseNativePreds = args.sliding(2).toList flatMap { case List(left, right) =>
      compareArgs(compareCond, staticIntCalc, staticFlonumCalc, left, right)(comparePlan) match {
        case UnplannableCompare =>
          // We can't compare this at compile time
          return None

        case StaticCompare(false) =>
          // This is false - the whole expression must be false!
          return Some(iv.ConstantBooleanValue(false))

        case StaticCompare(true) =>
          // We don't need to include constant true values
          None

        case DynamicCompare(nativePred) =>
          Some(nativePred)
      }
    }

    if (pairwiseNativePreds.isEmpty) {
      // This is statically true
      return Some(iv.ConstantBooleanValue(true))
    }

    // We definitely need to compare at runtime - include our plan steps
    plan.steps ++= comparePlan.steps

    // Combine all of the native predicates together
    val resultPred = pairwiseNativePreds.reduceLeft { (nativePred, trueBranchValue) =>
      val condResult = ps.TempValue()
      val valuePhi = ps.ValuePhi(condResult, trueBranchValue, nativePred)

      plan.steps += ps.CondBranch(nativePred, Nil, Nil, List(valuePhi))

      condResult
    }

    Some(new iv.NativePredicateValue(resultPred))
  }

  private def phiTypeForSelect(leftValue: iv.IntermediateValue, rightValue: iv.IntermediateValue): vt.ValueType = {
    (leftValue, rightValue) match {
      case (leftUnboxed: iv.NativeValue, rightUnboxed: iv.NativeValue)
          if leftUnboxed.nativeType == rightUnboxed.nativeType =>
        leftUnboxed.nativeType

      case _ =>
        leftValue.schemeType + rightValue.schemeType
    }
  }

  private def selectArgList(
      compareCond: ps.CompareCond,
      staticIntCalc: IntegerCompartor,
      staticFlonumCalc: DoubleCompartor,
      args: List[iv.IntermediateValue]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    // Compare in a fork in case we abort the whole thing later
    val comparePlan = plan.forkPlan()

    val definiteResult = args.reduceLeft { (left, right) =>
      // Compare these two values
      val compareResult = compareArgs(compareCond, staticIntCalc, staticFlonumCalc, left, right)(comparePlan)

      compareResult match {
        case UnplannableCompare =>
          // We can't compare this at compile time
          return None

        case StaticCompare(true) =>
          // Statically select the left value
          left

        case StaticCompare(false) =>
          // Statically select the right value
          right

        case DynamicCompare(nativePred) =>
          val truePlan = comparePlan.forkPlan()
          val falsePlan = comparePlan.forkPlan()

          // Phi the results together
          val resultType = phiTypeForSelect(left, right)
          val resultTemp = ps.TempValue()

          val trueResult = left.toTempValue(resultType)(truePlan)
          val falseResult = right.toTempValue(resultType)(falsePlan)

          val valuePhi = ps.ValuePhi(resultTemp, trueResult, falseResult)
          comparePlan.steps += ps.CondBranch(nativePred, truePlan.steps.toList, falsePlan.steps.toList, List(valuePhi))

          TempValueToIntermediate(resultType, resultTemp)
      }
    }

    // We can use the compare plan now
    plan.steps ++= comparePlan.steps
    Some(definiteResult)
  }

  /** Creates value constraints for numeric equality
    *
    * If at least one constant is being tested for equality we can propagate that constant's value to the other
    * arguments if the test returns true. There are a few tricky bits:
    *
    * - We can never replace a flonum value with a constant zero as `(=)` does not distinguish between +0.0 and -0.0.
    *   Integers only have one type of zero so they are not impacted.
    *
    * - NaNs are never considered `(=)`. This represents a missed opportunity to propagate NaN constants but does not
    *   impact correctness.
    *
    * - Integer constants can be used as a basis for flonum constants and vice versa. This is because their comparison
    *   happens by promoting both to a double and comparing for exact equality. The comparison will only succeed if the
    *   flonum has a bitwise identical value to the integer's promoted value.
    */
  private def constrainEqualArgsWithConstants(initialState: PlannerState)(
    predicateValue: iv.IntermediateValue,
    argValues: List[iv.IntermediateValue]
  ): PlannerState = argValues.collectFirst {
    case constantNumber: iv.ConstantNumberValue =>
      argValues.foldLeft(initialState) { case (state, argValue) =>
        val trueConstraint = if (argValue.hasDefiniteType(vt.IntegerType)) {
          ConstrainValue.SubstituteWithConstant(iv.ConstantIntegerValue(constantNumber.longValue))
        }
        else if (argValue.hasDefiniteType(vt.FlonumType) && (constantNumber.longValue != 0)) {
          ConstrainValue.SubstituteWithConstant(iv.ConstantFlonumValue(constantNumber.doubleValue))
        }
        else {
          ConstrainValue.PreserveValue
        }

        ConstrainValue.addCondAction(state)(
          conditionValue=predicateValue,
          ConstrainValue.CondAction(
            subjectValue=argValue,
            trueConstraint=trueConstraint,
            falseConstraint=ConstrainValue.PreserveValue
          )
        )
      }
  } getOrElse(initialState)

  override def planWithResult(initialState: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = (reportName, args) match {
    case ("=", args) if args.length >= 2 =>
      val argValues = args.map(_._2)

      compareArgList(ps.CompareCond.Equal, _ == _, _ == _, argValues).map { predicateValue =>
        val constrainedState = constrainEqualArgsWithConstants(initialState)(predicateValue, argValues)

        PlanResult(
          state=constrainedState,
          value=predicateValue
        )
      }

    case _ =>
      planWithValue(initialState)(reportName, args) map { value =>
        PlanResult(
          state=initialState,
          value=value
        )
      }
  }

  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case (">", args) if args.length >= 2 =>
      compareArgList(ps.CompareCond.GreaterThan, _ > _, _ > _, args.map(_._2))

    case (">=", args) if args.length >= 2 =>
      compareArgList(ps.CompareCond.GreaterThanEqual, _ >= _, _ >= _, args.map(_._2))

    case ("<", args) if args.length >= 2 =>
      compareArgList(ps.CompareCond.LessThan, _ < _, _ < _, args.map(_._2))

    case ("<=", args) if args.length >= 2 =>
      compareArgList(ps.CompareCond.LessThanEqual, _ <= _, _ <= _, args.map(_._2))

    case ("max" | "min", List((_, singleArg))) =>
      Some(singleArg.castToSchemeType(vt.NumberType))

    case ("max", args) if args.length > 1 =>
      selectArgList(ps.CompareCond.GreaterThan, _ > _, _ > _, args.map(_._2))

    case ("min", args) if args.length > 1 =>
      selectArgList(ps.CompareCond.LessThan, _ < _, _ < _, args.map(_._2))

    case ("integer", List(singleArg)) =>
      integerValue(singleArg._2)

    case ("flonum", List(singleArg)) =>
      flonumValue(singleArg._2)

    case _ =>
      None
  }
}
