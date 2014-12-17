package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object CharProcPlanner extends ReportProcPlanner {
  private type CharComparator = (Int, Int) => Boolean

  private def compareOperandList(
      compareCond : ps.CompareCond,
      staticCalc : CharComparator,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = {
    // Compare in a fork in case we abort the whole thing later
    val comparePlan = plan.forkPlan()

    // Compare the values pairwise
    val pairwiseNativePreds = operands.sliding(2).toList flatMap {
      case List(iv.ConstantCharValue(value1), iv.ConstantCharValue(value2)) =>
        if (!staticCalc(value1, value2)) {
          // This is statically false
          return Some(iv.ConstantBooleanValue(false))
        }
        else {
          None
        }

      case List(dynamic1, dynamic2) =>
        val val1Temp = dynamic1.toTempValue(vt.UnicodeChar)(comparePlan)
        val val2Temp = dynamic2.toTempValue(vt.UnicodeChar)(comparePlan)

        val predicateTemp = ps.Temp(vt.Predicate)

        val signed = if (compareCond == ps.CompareCond.Equal) {
          None
        }
        else {
          Some(true)
        }

        // Do a direct integer compare
        comparePlan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

        Some(predicateTemp)
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
      plan.steps += ps.CondBranch(condResult, nativePred, Nil, trueBranchValue, Nil, nativePred)

      condResult
    }

    Some(new iv.NativePredicateValue(resultPred))
  }

  override def planWithValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("char->integer", List((_, iv.ConstantCharValue(constantCharVal)))) =>
      Some(iv.ConstantExactIntegerValue(constantCharVal))

    case ("char->integer", List((charLocated, charValue))) =>
      val int32Temp = plan.withContextLocation(charLocated) {
        charValue.toTempValue(vt.UnicodeChar)
      }

      Some(new iv.NativeExactIntegerValue(int32Temp, vt.Int32))

    case ("integer->char", List((_, iv.ConstantExactIntegerValue(constantIntVal)))) =>
      Some(iv.ConstantCharValue(constantIntVal.toInt))

    case ("integer->char", List((intLocated, intValue))) =>
      val int32Temp = plan.withContextLocation(intLocated) {
        intValue.toTempValue(vt.Int32)
      }

      Some(new iv.NativeCharValue(int32Temp))

    case ("char=?", operands) if operands.length >= 2 =>
      compareOperandList(ps.CompareCond.Equal, _ == _, operands.map(_._2))

    case ("char>?", operands) if operands.length >= 2 =>
      compareOperandList(ps.CompareCond.GreaterThan,  _ > _, operands.map(_._2))

    case ("char>=?", operands) if operands.length >= 2 =>
      compareOperandList(ps.CompareCond.GreaterThanEqual, _ >= _, operands.map(_._2))

    case ("char<?", operands) if operands.length >= 2 =>
      compareOperandList(ps.CompareCond.LessThan, _ < _, operands.map(_._2))

    case ("char<=?", operands) if operands.length >= 2 =>
      compareOperandList(ps.CompareCond.LessThanEqual, _ <= _, operands.map(_._2))

    case _ =>
      None
  }
}
