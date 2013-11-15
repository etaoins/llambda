package llambda.et
import llambda._

sealed abstract trait Expression extends SourceLocated {
  val subexpressions : List[Expression]

  def map(f : Expression => Expression) : Expression
}

case class Begin(expressions : List[Expression]) extends Expression {
  val subexpressions = expressions

  def map(f : Expression => Expression) : et.Begin =
    et.Begin(expressions.map(f))
}

case class Apply(procedure : Expression, operands : List[Expression]) extends Expression {
  val subexpressions = procedure :: operands

  def map(f : Expression => Expression) : et.Apply =
    et.Apply(f(procedure), operands.map(f))

  override def toString = 
    // Make the operands follow the procedure directly like in Scheme
    "Apply(" + (procedure :: operands).mkString(", ") + ")"
}

case class VarRef(variable : StorageLocation) extends Expression {
  val subexpressions = Nil

  def map(f : Expression => Expression) : et.VarRef = this
}

case class MutateVar(variable : StorageLocation, value : Expression) extends Expression {
  val subexpressions = value :: Nil

  def map(f : Expression => Expression) : et.MutateVar =
    et.MutateVar(variable, f(value))
}

case class Literal(value : ast.Datum) extends Expression {
  val subexpressions = Nil

  def map(f : Expression => Expression) : et.Literal = this

  override def toString = "'" + value.toString
}

case class Cond(test : Expression, trueExpr : Expression, falseExpr : Expression) extends Expression {
  val subexpressions = test :: trueExpr :: falseExpr :: Nil

  def map(f : Expression => Expression) : et.Cond =
    et.Cond(f(test), f(trueExpr), f(falseExpr))
}

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], body : Expression) extends Expression {
  val subexpressions = body :: Nil

  def map(f : Expression => Expression) : et.Lambda =
    et.Lambda(fixedArgs, restArg, f(body))
}

case class Bind(bindings : List[(StorageLocation, Expression)]) extends Expression {
  val subexpressions = bindings.map(_._2)

  def map(f : Expression => Expression) : et.Bind =
    et.Bind(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    })
}

case class NativeFunction(
  fixedArgs : List[nfi.NativeType],
  hasRestArg : Boolean,
  returnType : Option[nfi.NativeType],
  nativeSymbol : String
) extends Expression with nfi.NativeSignature {
  val hasSelfArg = false

  val subexpressions = Nil
  def map(f : Expression => Expression) : et.NativeFunction = this
}
