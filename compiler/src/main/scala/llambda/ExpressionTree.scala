package llambda.et
import llambda._

sealed abstract trait Expression 

case class ProcedureCall(function : BoundValue, operands : List[Expression]) extends Expression 

case class VarReference(variable : BoundValue) extends Expression 

case class SetVar(variable : BoundValue, value : Expression) extends Expression

case class Literal(value : ast.Datum) extends Expression

case class Conditional(test : Expression, trueExpr : Expression, falseExpr : Option[Expression]) extends Expression 

case class Procedure(fixedArgs : List[ProcedureArg], restArg : Option[ProcedureArg], expressions : List[Expression]) extends Expression
