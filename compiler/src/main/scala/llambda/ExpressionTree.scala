package llambda.et
import llambda._

sealed abstract trait Expression 

case class Apply(function : Expression, operands : List[Expression]) extends Expression 

case class VarRef(variable : BoundValue) extends Expression 

case class MutateVar(variable : StorageLocation, value : Expression) extends Expression

case class Literal(value : ast.Datum) extends Expression

case class Cond(test : Expression, trueExpr : Expression, falseExpr : Expression) extends Expression 

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], expressions : List[Expression]) extends Expression

case class Let(bindings : List[(StorageLocation, Expression)], innerExprs : List[Expression]) extends Expression

case class NativeFunction(fixedArgs : List[nfi.NativeType], hasRestArg : Boolean, returnType : Option[nfi.NativeType], nativeSymbol : String) extends Expression
