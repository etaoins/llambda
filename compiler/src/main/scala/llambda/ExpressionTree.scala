package llambda.et

import llambda._
import llambda.{valuetype => vt}

sealed abstract trait Expression extends SourceLocated {
  val subexpressions : List[Expression]

  def map(f : Expression => Expression) : Expression
}

case class Begin(expressions : List[Expression]) extends Expression {
  val subexpressions = expressions

  def map(f : Expression => Expression) : et.Begin =
    et.Begin(expressions.map(f)).assignLocationFrom(this)
}

case class Apply(procedure : Expression, operands : List[Expression]) extends Expression {
  val subexpressions = procedure :: operands

  def map(f : Expression => Expression) : et.Apply =
    et.Apply(f(procedure), operands.map(f)).assignLocationFrom(this)

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
    et.MutateVar(variable, f(value)).assignLocationFrom(this)
}

case class Literal(value : ast.Datum) extends Expression {
  val subexpressions = Nil

  def map(f : Expression => Expression) : et.Literal = this

  override def toString = "'" + value.toString
}

case class Cond(test : Expression, trueExpr : Expression, falseExpr : Expression) extends Expression {
  val subexpressions = test :: trueExpr :: falseExpr :: Nil

  def map(f : Expression => Expression) : et.Cond =
    et.Cond(f(test), f(trueExpr), f(falseExpr)).assignLocationFrom(this)
}

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], body : Expression) extends Expression {
  val subexpressions = body :: Nil

  def map(f : Expression => Expression) : et.Lambda =
    et.Lambda(fixedArgs, restArg, f(body)).assignLocationFrom(this)
}

case class Bind(bindings : List[(StorageLocation, Expression)]) extends Expression {
  val subexpressions = bindings.map(_._2)

  def map(f : Expression => Expression) : et.Bind =
    et.Bind(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }).assignLocationFrom(this)
}

case class NativeFunction(
  fixedArgs : List[vt.ValueType],
  hasRestArg : Boolean,
  returnType : Option[vt.ValueType],
  nativeSymbol : String
) extends Expression with ProcedureSignature {
  val hasClosureArg = false

  val subexpressions = Nil
  def map(f : Expression => Expression) : et.NativeFunction = this
}

sealed abstract class RecordTypeProcedure extends Expression {
  val recordType : vt.RecordCellType

  val subexpressions = Nil
  def map(f : Expression => Expression) : this.type = this
}

case class RecordTypeConstructor(recordType : vt.RecordCellType, initializedFields : List[vt.RecordField]) extends RecordTypeProcedure
case class RecordTypePredicate(recordType : vt.RecordCellType) extends RecordTypeProcedure
case class RecordTypeAccessor(recordType : vt.RecordCellType, field : vt.RecordField) extends RecordTypeProcedure
case class RecordTypeMutator(recordType : vt.RecordCellType, field : vt.RecordField) extends RecordTypeProcedure
