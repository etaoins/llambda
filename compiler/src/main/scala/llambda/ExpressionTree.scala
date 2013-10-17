package llambda.et
import llambda._

sealed abstract trait Expression {
  val subexpressions : List[Expression]
}

case class Begin(expressions : List[Expression]) extends Expression {
  val subexpressions = expressions
}

case class Apply(procedure : Expression, operands : List[Expression]) extends Expression {
  val subexpressions  = procedure :: operands
}

case class VarRef(variable : BoundValue) extends Expression {
  val subexpressions = Nil
}

case class MutateVar(variable : StorageLocation, value : Expression) extends Expression {
  val subexpressions = value :: Nil
}

case class Literal(value : ast.Datum) extends Expression {
  val subexpressions = Nil
}

case class Cond(test : Expression, trueExpr : Expression, falseExpr : Expression) extends Expression {
  val subexpressions = test :: trueExpr :: falseExpr :: Nil
}

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], body : Expression) extends Expression {
  val subexpressions = body :: Nil
}

case class Bind(bindings : List[(StorageLocation, Expression)]) extends Expression {
  val subexpressions = bindings.map(_._2)
}

case class NativeFunction(
  fixedArgs : List[nfi.NativeType],
  hasRestArg : Boolean,
  returnType : Option[nfi.NativeType],
  nativeSymbol : String
) extends Expression with nfi.NativeSignature {
  val hasSelfArg = false

  val subexpressions = Nil
}
