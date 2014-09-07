package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.ReturnType

object ValuesProcPlanner extends ReportProcPlanner {
  def apply(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[ResultValues] = (reportName, operands) match {
    case ("values", List(singleOperand)) =>
      Some(SingleValue(singleOperand._2))

    case ("values", operands) =>
      // We have a specific type here!
      val operandValues = operands.map(_._2)
      val operandTypes = operandValues.map(_.schemeType)

      val returnType = ReturnType.SpecificValues(operandTypes)
      val representationType = returnType.representationType

      // Create a list with the correct represention type
      val returnListTemp = ValuesToProperList(operandValues).toTempValue(returnType.representationType)
      val returnListBoxed = BoxedValue(representationType.cellType, returnListTemp)

      Some(SpecificValues(returnListBoxed, operandTypes))

    case ("call-with-values", List(
        (producerContextLoc, producerValue),
        (consumerContextLoc, consumerValue)
    )) =>
      val invokableProducer = plan.withContextLocation(producerContextLoc) {
        producerValue.toInvokableProcedure
      }

      val invokableConsumer = plan.withContextLocation(producerContextLoc) {
        consumerValue.toInvokableProcedure
      }

      // Get the producer values
      val values = PlanInvokeApply.withIntermediateValues(invokableProducer, Nil)

      // Invoke the consumer
      val valuesReturnList = values.toReturnIntermediateValue(ReturnType.ArbitraryValues).get
      val consumerValues = PlanInvokeApply.withArgumentList(invokableConsumer, valuesReturnList)

      Some(consumerValues)

    case _ =>
      None
  }
}
