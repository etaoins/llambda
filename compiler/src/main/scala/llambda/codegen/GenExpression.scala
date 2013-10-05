package llambda.codegen

import llambda.et
import llambda.codegen.llvmir._

case class ExpressionResult(
  state : GenerationState,
  value : LiveValue)

object GenExpression {
  def apply(initialState : GenerationState)(expr : et.Expression) : ExpressionResult = {
    expr match {
      case et.Literal(datum) =>
        ExpressionResult(state=initialState, value=GenLiteral(datum))

      case nativeFunc : et.NativeFunction =>
        val funcValue = GenNativeFunction(initialState.module)(nativeFunc) 
        ExpressionResult(state=initialState, value=funcValue)

      case et.Apply(procedure, operands) =>
        // Generate the procedure value
        val procResult = apply(initialState)(procedure)
        val procValue = procResult.value

        // Now generate each operand
        val (operandState, operandValues) = operands.foldLeft((procResult.state, List[LiveValue]())) { case ((state, operandValues), operand) =>
          val operandResult = GenExpression(state)(operand)

          (operandResult.state, operandValues :+ operandResult.value)
        }

        GenApplication(operandState)(procValue, operandValues)
    }
  }
}
