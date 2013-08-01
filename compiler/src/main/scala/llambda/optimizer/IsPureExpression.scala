package llambda.optimizer

import llambda.et

object IsPureExpression {
  def apply(expression : et.Expression) : Boolean = {
    expression match {
      case _ : et.VarRef =>
        // Variable references are impure because we currently have no way of
        // determining if the referenced variable is mutable.
        false

      case _ : et.MutateVar =>
        // Mutating a variable is always impure
        false

      case _ : et.Lambda =>
        // Lambda expressions are always pure
        // Note that their application may not be
        true

      case _ : et.NativeFunction =>
        // Native function expressions are always pure
        true

      case et.Apply(lambda : et.Lambda, operands) =>
        // Applying lambdas is only pure if both the operands and inner expressions are pure
        lambda.expressions.forall(apply) && operands.forall(apply)
      
      case _ : et.Apply =>
        // Any other application is impure
        false

      case _ : et.Literal =>
        // Literals are always pure
        true

      case et.Let(bindings, innerExprs) =>
        // Lets are pure if all their bindings and inner expressions are pure
        bindings.map(_._2).forall(apply) && innerExprs.forall(apply)

      case et.Cond(test, trueExpr, falseExpr) =>
        // Conditionals are pure if the test and expressions are pure
        List(test, trueExpr, falseExpr).forall(apply)
    }
  }
}
