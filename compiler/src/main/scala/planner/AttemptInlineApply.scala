package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{StorageLocation, ContextLocated}
import llambda.compiler.IncompatibleArityException
import llambda.compiler.et

import scala.collection.breakOut

private[planner] object AttemptInlineApply {
  def apply(parentState : PlannerState)(lambdaExpr : et.Lambda, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    val mutableVars = plan.config.analysis.mutableVars
    val allArgs = lambdaExpr.fixedArgs ++ lambdaExpr.restArg

    if (!(mutableVars & allArgs.toSet).isEmpty) {
      // Not supported yet
      return None
    }

    if ((operands.length < lambdaExpr.fixedArgs.length) ||
        ((operands.length > lambdaExpr.fixedArgs.length) && !lambdaExpr.restArg.isDefined)) {
      // Incompatible arity - let PlanInvokeApply fail this
      return None
    }

    if (ContainsImmediateReturn(lambdaExpr.body)) {
      // Not supported yet
      return None
    }
    
    val closedVars = FindClosedVars(parentState, lambdaExpr.body)

    // We only support immutables at this point
    val importedValues = closedVars collect {
      case ImportedImmutable(storageLoc, parentIntermediate) =>
        (storageLoc -> ImmutableValue(parentIntermediate))

      case _ =>
        // Not supported
        return None
    }

    // Convert our arguments to ImmutableValues
    val fixedArgImmutables = (lambdaExpr.fixedArgs.zip(operands).map { case (storageLoc, (_, argValue)) =>
      if (argValue.schemeType.satisfiesType(storageLoc.schemeType) != Some(true)) {
        // This type cast could fail at runtime
        return None
      }

      (storageLoc -> ImmutableValue(argValue))
    })(breakOut) : Map[StorageLocation, LocationValue]
    
    // We only support empty rest args at this point
    val restArgImmutables = lambdaExpr.restArg.map { restArgLoc =>
      val restValues = operands.drop(lambdaExpr.fixedArgs.length).map(_._2)
      restArgLoc -> ImmutableValue(ValuesToProperList(restValues))
    }

    // Map our input immutables to their new storage locations
    val inlineBodyState = PlannerState(
      values=fixedArgImmutables ++ restArgImmutables ++ importedValues,
      worldPtr=worldPtr
    )

    val planResult = PlanExpr(inlineBodyState)(lambdaExpr.body)
    Some(planResult.value)
  }
}
