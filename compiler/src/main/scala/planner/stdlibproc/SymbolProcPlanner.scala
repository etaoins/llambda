package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.ContextLocated

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}


object SymbolProcPlanner extends StdlibProcPlanner {
  /** Heuristically ranks the difficulty of comparing this symbol ascending order
    *
    * When comparing more than two symbols there are multiple possible sets of comparisons that can be used to prove
    * that all the symbols are equal. This is used to attempt to generate the least expensive set of comparisons.
    */
  private def rankSymbolDifficulty(value: iv.IntermediateValue): Long = value.schemeType match {
    case vt.LiteralSymbolType(_) => 0L
    case unionType: vt.UnionType => unionType.memberTypes.size + (1L << 32)
    case _ =>                       Long.MaxValue
  }

  private def compareSymbolList(initialState: PlannerState)(
      values: List[iv.IntermediateValue]
  )(implicit plan: PlanWriter): Option[PlanResult] = {
    // This will collapse equal literals and values from the same storage location
    val distinctValues = values.distinct.map(_.castToSchemeType(vt.SymbolType))
    val rankedValues = distinctValues.sortBy(rankSymbolDifficulty)

    val commonType = rankedValues.map(_.schemeType).reduce(_ & _)

    if (commonType == vt.EmptySchemeType) {
      // We must be inequal
      return Some(PlanResult(
        state=initialState,
        value=iv.ConstantBooleanValue(false)
      ))
    }

    val predicateValue = rankedValues.sortBy(rankSymbolDifficulty) match {
      case easiestSymbol :: otherSymbol :: rest =>
        val argPreds = (otherSymbol :: rest) map { testSymbol =>
          PlanSymbolEquality.compareDynamic(easiestSymbol, testSymbol)
        }

        val resultPred = argPreds.reduceLeft { (pred1, pred2) =>
          val condResult = ps.TempValue()

          // If pred1 is false we'll return it directly; otherwise return pred2
          val valuePhi = ps.ValuePhi(condResult, pred2, pred1)
          plan.steps += ps.CondBranch(pred1, Nil, Nil, List(valuePhi))

          condResult
        }

        new iv.NativePredicateValue(resultPred)

      case _ =>
        iv.ConstantBooleanValue(true)
    }

    // If we're comparing a single value against a constant symbol we can subtract its type
    val otherConstantSymbolValueOpt = rankedValues match {
      case List(constantSymbolValue: iv.ConstantSymbolValue, _) =>
        Some(constantSymbolValue)

      case _ =>
        None
    }

    // Constrain the state
    val constrainedState = rankedValues.foldLeft(initialState) { (state, value) =>
      val falseConstraint = otherConstantSymbolValueOpt match {
        case Some(constantSymbolValue) if constantSymbolValue != value =>
          ConstrainValue.SubtractType(constantSymbolValue.schemeType)
        case _ =>
          ConstrainValue.PreserveValue
      }

      ConstrainValue.addCondAction(state)(
        conditionValue=predicateValue,
        ConstrainValue.CondAction(
          subjectValue=value,
          trueConstraint=ConstrainValue.IntersectType(commonType),
          falseConstraint=falseConstraint
        )
      )
    }

    Some(PlanResult(
      state=constrainedState,
      value=predicateValue
    ))
  }

  override def planWithResult(initialState: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[PlanResult] = (reportName, args) match {
    case ("symbol=?", args) if args.length >= 2 =>
      val argValues = args.map(_._2.castToSchemeType(vt.SymbolType))
      compareSymbolList(initialState)(argValues)

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
    case ("string->symbol", List((_, iv.ConstantStringValue(constValue)))) =>
      Some(iv.ConstantSymbolValue(constValue))

    case ("symbol->string", List((_, iv.ConstantSymbolValue(constValue)))) =>
      Some(iv.ConstantStringValue(constValue))

    case _ =>
      None
  }
}
