package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object NumberProcPlanner extends StdlibProcPlanner {
  private type IntegerCompartor = (Long, Long) => Boolean

  private type DoubleCompartor = (Double, Double) => Boolean

  private type StaticIntegerOp = (Long, Long) => Long
  private type StaticDoubleOp = (Double, Double) => Double

  private sealed abstract class CompareResult
  private case class StaticCompare(result : Boolean) extends CompareResult
  private case class DynamicCompare(nativePred : ps.TempValue) extends CompareResult
  private case object UnplannableCompare extends CompareResult

  private def integerValue(
      value : iv.IntermediateValue
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
      value match  {
        case knownInt if knownInt.hasDefiniteType(vt.IntegerType) =>
          // Already an  int
          Some(knownInt)

        case iv.ConstantFlonumValue(constFlonumVal) =>
          val longValue = constFlonumVal.toLong

          // Make sure this was lossless
          if (longValue.toDouble == constFlonumVal) {
            Some(iv.ConstantIntegerValue(longValue))
          }
          else {
            None
          }

        case _ =>
          None
      }
  }

  private def flonumValue(
      value : iv.IntermediateValue
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
      value match  {
        case knownFlonum if knownFlonum.hasDefiniteType(vt.FlonumType) =>
          // Already a flonum
          Some(knownFlonum)

        case iv.ConstantIntegerValue(constIntegerVal) =>
          // Statically convert it to a double
          Some(iv.ConstantFlonumValue(constIntegerVal.toDouble))

        case knownInt if knownInt.hasDefiniteType(vt.IntegerType) =>
          val intTemp = knownInt.toTempValue(vt.IntegerType)
          val doubleTemp = ps.Temp(vt.Double)

          plan.steps += ps.ConvertNativeIntegerToFloat(doubleTemp, intTemp, true, vt.Double)

          Some(new iv.NativeFlonumValue(doubleTemp, vt.Double))

        case _ =>
          None
      }
  }

  private def compareArgs(
      compareCond : ps.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticFlonumCalc : DoubleCompartor,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue
  )(implicit plan : PlanWriter) : CompareResult = {
    (val1, val2) match {
      case (iv.ConstantIntegerValue(constIntegerVal1), iv.ConstantIntegerValue(constIntegerVal2)) =>
        val compareResult = staticIntCalc(constIntegerVal1, constIntegerVal2)
        StaticCompare(compareResult)

      case (constNum1 : iv.ConstantNumberValue, constNum2 : iv.ConstantNumberValue) =>
        val compareResult = staticFlonumCalc(constNum1.doubleValue, constNum2.doubleValue)
        StaticCompare(compareResult)

      case (integer1, integer2) if integer1.hasDefiniteType(vt.IntegerType) && integer2.hasDefiniteType(vt.IntegerType) =>
        val val1Temp = integer1.toTempValue(vt.Int64)
        val val2Temp = integer2.toTempValue(vt.Int64)

        val predicateTemp = ps.Temp(vt.Predicate)

        val signed = if (compareCond == ps.CompareCond.Equal) {
          None
        }
        else {
          Some(true)
        }

        // Do a direct integer compare
        plan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

        DynamicCompare(predicateTemp)

      case (flonum1, flonum2) if flonum1.hasDefiniteType(vt.FlonumType) && flonum2.hasDefiniteType(vt.FlonumType) =>
        val val1Temp = flonum1.toTempValue(vt.Double)
        val val2Temp = flonum2.toTempValue(vt.Double)

        val predicateTemp = ps.Temp(vt.Predicate)

        // Do a direct float compare
        plan.steps += ps.FloatCompare(predicateTemp, compareCond, val1Temp, val2Temp)

        DynamicCompare(predicateTemp)

      case _ =>
        UnplannableCompare
    }
  }

  private def compareArgList(
      compareCond : ps.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticFlonumCalc : DoubleCompartor,
      args : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    // Compare in a fork in case we abort the whole thing later
    val comparePlan = plan.forkPlan()

    val pairwiseResults = args.sliding(2).toList map {
      case List(left, right) =>
        compareArgs(compareCond, staticIntCalc, staticFlonumCalc, left, right)(comparePlan)
    }

    // Now filter out all the static results
    val pairwiseNativePreds = pairwiseResults flatMap {
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
    } : List[ps.TempValue]

    if (pairwiseNativePreds.isEmpty) {
      // This is statically true
      return Some(iv.ConstantBooleanValue(true))
    }

    // We definitely need to compare at runtime - include our plan steps
    plan.steps ++= comparePlan.steps

    // Combine all of the native predicates together
    val resultPred = pairwiseNativePreds.reduceLeft { (nativePred, trueBranchValue) =>
      val condResult = ps.Temp(vt.Predicate)
      val valuePhi = ps.ValuePhi(condResult, trueBranchValue, nativePred)

      plan.steps += ps.CondBranch(nativePred, Nil, Nil, List(valuePhi))

      condResult
    }

    Some(new iv.NativePredicateValue(resultPred))
  }

  private def phiTypeForSelect(leftValue : iv.IntermediateValue, rightValue : iv.IntermediateValue) : vt.ValueType = {
    (leftValue, rightValue) match {
      case (leftUnboxed : iv.NativeValue, rightUnboxed : iv.NativeValue)
          if leftUnboxed.nativeType == rightUnboxed.nativeType =>
        leftUnboxed.nativeType

      case _ =>
        leftValue.schemeType + rightValue.schemeType
    }
  }

  private def selectArgList(
      compareCond : ps.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticFlonumCalc : DoubleCompartor,
      args : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
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
          val resultTemp = ps.Temp(resultType)

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

  override def planWithValue(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("=", args) if args.length >= 2 =>
      compareArgList(ps.CompareCond.Equal, _ == _, _ == _, args.map(_._2))

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
