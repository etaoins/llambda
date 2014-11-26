package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.et
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ValuesProcPlanner extends ReportProcPlanner {
  override def planFromExprs(initialState : PlannerState)(
      reportName : String,
      operands : List[et.Expr]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("call-with-values", List(producerExpr, consumerExpr))  =>
      // Call the producer, possibly while inlining
      val producerResult = plan.withContextLocation(producerExpr) {
        PlanApplication(initialState)(producerExpr, Nil)
      }

      producerResult.values.toMultipleValueList() match {
        case knownListElement : iv.KnownListElement =>
          knownListElement.toValueListOpt map { argValues =>
            // We statically know our arguments!
            val locatedArgValues = argValues.map((producerExpr, _))

            plan.withContextLocation(consumerExpr) {
              PlanApplication.planWithOperandValues(producerResult.state)(consumerExpr, locatedArgValues)
            }
          }

        case otherArgList =>
          // This is by definition a list but we don't know the contents of the list
          // We need to fall back to PlanInokeApply on the consumer to treat it like a dynamic (apply)
          val consumerResult = PlanExpr(producerResult.state)(consumerExpr)
          val invokableConsumer = plan.withContextLocation(consumerExpr) {
            consumerResult.values.toSingleValue.toInvokableProcedure
          }

          Some(PlanResult(
            state=consumerResult.state,
            values=PlanInvokeApply.withArgumentList(invokableConsumer, otherArgList)
          ))
      }

    case _ =>
      None
  }

  override def planWithValues(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("values", operands) =>
      // We have a specific type here!
      val operandValues = operands.map(_._2)
      Some(ResultValues(operandValues))

    case _ =>
      None
  }
}
