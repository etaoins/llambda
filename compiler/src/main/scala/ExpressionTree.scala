package io.llambda.compiler.et
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

sealed abstract trait Expression extends SourceLocated {
  val subexpressions : List[Expression]

  def map(f : Expression => Expression) : Expression

  def toSequence : List[Expression] = {
    List(this)
  }
}

/**
 * Represents any expression except for Literal
 *
 * This is used by PartialValue in the reducer
 */
sealed abstract trait NonLiteralExpression extends Expression

object Expression {
  def fromSequence(exprs : List[Expression]) : Expression = exprs match {
    // Wrap our expressions in an Begin unless there's exactly one
    // This isn't required but produces more readable ETs and unit tests
    case singleValue :: Nil => 
      singleValue
    case otherValues =>
      Begin(otherValues)
  }
}

case class Begin(expressions : List[Expression]) extends NonLiteralExpression {
  val subexpressions = expressions

  def map(f : Expression => Expression) : Begin =
    Begin(expressions.map(f)).assignLocationFrom(this)

  override def toSequence : List[Expression] = 
    subexpressions.flatMap(_.toSequence)
}

case class Apply(procedure : Expression, operands : List[Expression]) extends NonLiteralExpression {
  val subexpressions = procedure :: operands

  def map(f : Expression => Expression) : Apply =
    Apply(f(procedure), operands.map(f)).assignLocationFrom(this)

  override def toString = 
    // Make the operands follow the procedure directly like in Scheme
    "Apply(" + (procedure :: operands).mkString(", ") + ")"
}

case class VarRef(variable : StorageLocation) extends NonLiteralExpression {
  val subexpressions = Nil

  def map(f : Expression => Expression) : VarRef = this
}

case class MutateVar(variable : StorageLocation, value : Expression) extends NonLiteralExpression {
  val subexpressions = value :: Nil

  def map(f : Expression => Expression) : MutateVar =
    MutateVar(variable, f(value)).assignLocationFrom(this)
}

case class Literal(value : ast.Datum) extends Expression {
  val subexpressions = Nil

  def map(f : Expression => Expression) : Literal = this

  override def toString = "'" + value.toString
}

case class Cond(test : Expression, trueExpr : Expression, falseExpr : Expression) extends NonLiteralExpression {
  val subexpressions = test :: trueExpr :: falseExpr :: Nil

  def map(f : Expression => Expression) : Cond =
    Cond(f(test), f(trueExpr), f(falseExpr)).assignLocationFrom(this)
}

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], body : Expression) extends NonLiteralExpression {
  val subexpressions = body :: Nil

  def map(f : Expression => Expression) : Lambda =
    Lambda(fixedArgs, restArg, f(body)).assignLocationFrom(this)
}

sealed abstract trait BindingExpression extends NonLiteralExpression {
  val bindings : List[(StorageLocation, Expression)]
}

case class TopLevelDefinition(bindings : List[(StorageLocation, Expression)]) extends BindingExpression {
  val subexpressions = bindings.map(_._2)

  def map(f : Expression => Expression) : TopLevelDefinition =
    TopLevelDefinition(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }).assignLocationFrom(this)
}

case class InternalDefinition(bindings : List[(StorageLocation, Expression)], body : Expression) extends BindingExpression {
  val subexpressions = body :: bindings.map(_._2)

  def map(f : Expression => Expression) : InternalDefinition =
    InternalDefinition(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }, f(body)).assignLocationFrom(this)
}

case class NativeFunction(
  signature : ProcedureSignature,
  nativeSymbol : String
) extends NonLiteralExpression {
  val subexpressions = Nil
  def map(f : Expression => Expression) : NativeFunction = this
}

sealed abstract class RecordTypeProcedure extends NonLiteralExpression {
  val recordType : vt.RecordType

  val subexpressions = Nil
  def map(f : Expression => Expression) : this.type = this
}

case class RecordTypeConstructor(recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends RecordTypeProcedure
case class RecordTypePredicate(recordType : vt.RecordType) extends RecordTypeProcedure
case class RecordTypeAccessor(recordType : vt.RecordType, field : vt.RecordField) extends RecordTypeProcedure
case class RecordTypeMutator(recordType : vt.RecordType, field : vt.RecordField) extends RecordTypeProcedure

case class Cast(valueExpr : Expression, targetType : vt.ValueType) extends NonLiteralExpression {
  val subexpressions = valueExpr :: Nil

  def map(f : Expression => Expression) : Cast =
    Cast(f(valueExpr), targetType).assignLocationFrom(this)
}

case class Parameterize(parameterValues : List[(Expression, Expression)], body : Expression) extends NonLiteralExpression {
  lazy val subexpressions = body :: parameterValues.flatMap { case (parameter, value) =>
    List(parameter, value)
  }

  def map(f : Expression => Expression) : Parameterize = {
    val newParams = parameterValues map { case (parameter, value) =>
      (f(parameter), f(value))
    }

    Parameterize(newParams, f(body))
  }
}

/**
 * Returns from the current lambda
 */
case class Return(value : Expression) extends NonLiteralExpression {
  lazy val subexpressions = List(value)

  def map(f : Expression => Expression) : Return = 
    Return(f(value))
}
