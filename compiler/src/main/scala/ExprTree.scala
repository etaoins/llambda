package io.llambda.compiler.et
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

sealed abstract trait Expr extends SourceLocated {
  val subexprs : List[Expr]

  def map(f : Expr => Expr) : Expr

  def toSequence : List[Expr] = {
    List(this)
  }
}

/** Represents any expression except for Literal
  *
  * This is used by reducer.PartialValue to define ReducedExpr
  */
sealed abstract trait NonLiteralExpr extends Expr

object Expr {
  def fromSequence(exprs : List[Expr]) : Expr = exprs match {
    // Wrap our expressions in an Begin unless there's exactly one
    // This isn't required but produces more readable ETs and unit tests
    case singleValue :: Nil => 
      singleValue
    case otherValues =>
      Begin(otherValues)
  }
}

case class Begin(exprs : List[Expr]) extends NonLiteralExpr {
  val subexprs = exprs

  def map(f : Expr => Expr) : Begin =
    Begin(exprs.map(f)).assignLocationFrom(this)

  override def toSequence : List[Expr] = 
    subexprs.flatMap(_.toSequence)
}

case class Apply(procedure : Expr, operands : List[Expr]) extends NonLiteralExpr {
  val subexprs = procedure :: operands

  def map(f : Expr => Expr) : Apply =
    Apply(f(procedure), operands.map(f)).assignLocationFrom(this)

  override def toString = 
    // Make the operands follow the procedure directly like in Scheme
    "Apply(" + (procedure :: operands).mkString(", ") + ")"
}

case class VarRef(variable : StorageLocation) extends NonLiteralExpr {
  val subexprs = Nil

  def map(f : Expr => Expr) : VarRef = this
}

case class MutateVar(variable : StorageLocation, value : Expr) extends NonLiteralExpr {
  val subexprs = value :: Nil

  def map(f : Expr => Expr) : MutateVar =
    MutateVar(variable, f(value)).assignLocationFrom(this)
}

case class Literal(value : ast.Datum) extends Expr {
  val subexprs = Nil

  def map(f : Expr => Expr) : Literal = this

  override def toString = "'" + value.toString
}

case class Cond(test : Expr, trueExpr : Expr, falseExpr : Expr) extends NonLiteralExpr {
  val subexprs = test :: trueExpr :: falseExpr :: Nil

  def map(f : Expr => Expr) : Cond =
    Cond(f(test), f(trueExpr), f(falseExpr)).assignLocationFrom(this)
}

case class Lambda(fixedArgs : List[StorageLocation], restArg : Option[StorageLocation], body : Expr) extends NonLiteralExpr {
  val subexprs = body :: Nil

  /** Hint that this in an artificially generated function for debugging information */
  var isArtificial : Boolean = false

  def map(f : Expr => Expr) : Lambda =
    Lambda(fixedArgs, restArg, f(body)).assignLocationFrom(this)
}

sealed abstract trait BindingExpr extends NonLiteralExpr {
  val bindings : List[(StorageLocation, Expr)]
}

case class TopLevelDefinition(bindings : List[(StorageLocation, Expr)]) extends BindingExpr {
  val subexprs = bindings.map(_._2)

  def map(f : Expr => Expr) : TopLevelDefinition =
    TopLevelDefinition(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }).assignLocationFrom(this)
}

case class InternalDefinition(bindings : List[(StorageLocation, Expr)], body : Expr) extends BindingExpr {
  val subexprs = body :: bindings.map(_._2)

  def map(f : Expr => Expr) : InternalDefinition =
    InternalDefinition(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }, f(body)).assignLocationFrom(this)
}

case class NativeFunction(
  signature : ProcedureSignature,
  nativeSymbol : String
) extends NonLiteralExpr {
  val subexprs = Nil
  def map(f : Expr => Expr) : NativeFunction = this
}

sealed abstract class RecordTypeProcedure extends NonLiteralExpr {
  val recordType : vt.RecordType

  val subexprs = Nil
  def map(f : Expr => Expr) : this.type = this
}

case class RecordTypeConstructor(recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends RecordTypeProcedure
case class RecordTypePredicate(recordType : vt.RecordType) extends RecordTypeProcedure
case class RecordTypeAccessor(recordType : vt.RecordType, field : vt.RecordField) extends RecordTypeProcedure
case class RecordTypeMutator(recordType : vt.RecordType, field : vt.RecordField) extends RecordTypeProcedure

case class Cast(valueExpr : Expr, targetType : vt.ValueType) extends NonLiteralExpr {
  val subexprs = valueExpr :: Nil

  def map(f : Expr => Expr) : Cast =
    Cast(f(valueExpr), targetType).assignLocationFrom(this)
}

case class Parameterize(parameterValues : List[(Expr, Expr)], body : Expr) extends NonLiteralExpr {
  lazy val subexprs = body :: parameterValues.flatMap { case (parameter, value) =>
    List(parameter, value)
  }

  def map(f : Expr => Expr) : Parameterize = {
    val newParams = parameterValues map { case (parameter, value) =>
      (f(parameter), f(value))
    }

    Parameterize(newParams, f(body)).assignLocationFrom(this)
  }
}

/** Returns from the current lambda with the given value */
case class Return(value : Expr) extends NonLiteralExpr {
  lazy val subexprs = List(value)

  def map(f : Expr => Expr) : Return = 
    Return(f(value)).assignLocationFrom(this)
}
