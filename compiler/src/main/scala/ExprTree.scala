package io.llambda.compiler.et
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

sealed abstract trait Expr extends ContextLocated {
  val subexprs : List[Expr]

  def map(f : Expr => Expr) : Expr
  
  def toSequence : List[Expr] = {
    List(this)
  }
  
  /** Version of map() that's guaranteed to clone the expr
    *
    * This is needed to support asInlinedFrom
    */
  protected def cloningMap(f : Expr => Expr) : Expr =
    map(f)

  /** Creates a copy of the expression tree annotated with the passed inline path entry
    *
    * This is used to generate accurate debugging information for macros and inlined lambdas
    */
  def asInlined(pathEntry : InlinePathEntry) : et.Expr = {
    val recursedExpr = this.cloningMap(_.asInlined(pathEntry))

    recursedExpr.inlinePath = recursedExpr.inlinePath :+ pathEntry
    recursedExpr
  }

  /** Returns the Scheme result type of this expression
    *
    * This does no recursive analysis like the or planner would. For example, all appplications have the result of
    * AnySchemeType. This is used to discard unused top-level expressions early if possible
    */
  def schemeType : vt.SchemeType = vt.AnySchemeType
}

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

case class Begin(exprs : List[Expr]) extends Expr {
  val subexprs = exprs

  def map(f : Expr => Expr) : Begin =
    Begin(exprs.map(f)).assignLocationFrom(this)

  override def toSequence : List[Expr] = 
    subexprs.flatMap(_.toSequence)
}

case class Apply(procedure : Expr, operands : List[Expr]) extends Expr {
  val subexprs = procedure :: operands

  def map(f : Expr => Expr) : Apply =
    Apply(f(procedure), operands.map(f)).assignLocationFrom(this)

  override def toString = 
    // Make the operands follow the procedure directly like in Scheme
    "Apply(" + (procedure :: operands).mkString(", ") + ")"
}

case class VarRef(variable : StorageLocation) extends Expr {
  val subexprs = Nil

  def map(f : Expr => Expr) : VarRef = this

  override def cloningMap(f : Expr => Expr) : Expr =
    VarRef(variable).assignLocationFrom(this)
  
  override def schemeType = variable.schemeType
}

case class MutateVar(variable : StorageLocation, value : Expr) extends Expr {
  val subexprs = value :: Nil

  def map(f : Expr => Expr) : MutateVar =
    MutateVar(variable, f(value)).assignLocationFrom(this)
}

case class Literal(value : ast.Datum) extends Expr {
  val subexprs = Nil

  def map(f : Expr => Expr) : Literal = this
  
  override def cloningMap(f : Expr => Expr) : Expr =
    Literal(value).assignLocationFrom(this)

  override def toString = "'" + value.toString
  
  override def schemeType = value.schemeType
}

case class Cond(test : Expr, trueExpr : Expr, falseExpr : Expr) extends Expr {
  val subexprs = test :: trueExpr :: falseExpr :: Nil

  def map(f : Expr => Expr) : Cond =
    Cond(f(test), f(trueExpr), f(falseExpr)).assignLocationFrom(this)

  override def schemeType = trueExpr.schemeType + falseExpr.schemeType
}

case class Lambda(
    override val schemeType : vt.ProcedureType,
    fixedArgs : List[StorageLocation],
    restArgOpt : Option[StorageLocation],
    body : Expr,
    debugContextOpt : Option[debug.SubprogramContext] = None
) extends Expr {
  val subexprs = List(body)

  def map(f : Expr => Expr) : Lambda =
    Lambda(schemeType, fixedArgs, restArgOpt, f(body), debugContextOpt).assignLocationFrom(this)
}

case class CaseLambda(
    clauses : List[Lambda]
) extends Expr {
  val subexprs = clauses 

  def map(f : Expr => Expr) : CaseLambda =
    CaseLambda(clauses.map(_.map(f))).assignLocationFrom(this)

  override def schemeType =
    vt.CaseProcedureType(clauses.map(_.schemeType))
}

case class TopLevelDefine(bindings : List[(StorageLocation, Expr)]) extends Expr {
  val subexprs = bindings.map(_._2)

  def map(f : Expr => Expr) : TopLevelDefine =
    TopLevelDefine(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }).assignLocationFrom(this)
}

case class InternalDefine(bindings : List[(StorageLocation, Expr)], body : Expr) extends Expr {
  val subexprs = body :: bindings.map(_._2)

  def map(f : Expr => Expr) : InternalDefine =
    InternalDefine(bindings.map { case (storageLoc, expr) =>
      (storageLoc, f(expr))
    }, f(body)).assignLocationFrom(this)
}

case class NativeFunction(
  signature : ProcedureSignature,
  nativeSymbol : String
) extends Expr {
  val subexprs = Nil

  def map(f : Expr => Expr) : NativeFunction = this

  override def cloningMap(f : Expr => Expr) : Expr =
    NativeFunction(signature, nativeSymbol).assignLocationFrom(this)

  override def schemeType = 
    signature.toSchemeProcedureType
}

/** Artificial procedures are created internally by the compiler
  *
  * They do not correspond to a defined procedure in the Scheme source
  */
sealed abstract class ArtificialProcedure extends Expr {
  val subexprs = Nil
  def map(f : Expr => Expr) : this.type = this
}

sealed abstract class RecordProcedure extends ArtificialProcedure {
  val recordType : vt.RecordType
}

case class RecordConstructor(recordType : vt.RecordType, initializedFields : List[vt.RecordField]) extends RecordProcedure {
  override def cloningMap(f : Expr => Expr) : Expr =
    RecordConstructor(recordType, initializedFields).assignLocationFrom(this)
  
  override def schemeType = vt.ProcedureType(
    fixedArgTypes=initializedFields.map(_.fieldType.schemeType),
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.SingleValue(recordType)
  )
}

case class RecordAccessor(recordType : vt.RecordType, field : vt.RecordField) extends RecordProcedure {
  override def cloningMap(f : Expr => Expr) : Expr =
    RecordAccessor(recordType, field).assignLocationFrom(this)
  
  override def schemeType = vt.ProcedureType(
    fixedArgTypes=List(recordType),
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.SingleValue(field.fieldType.schemeType)
  )
}

case class RecordMutator(recordType : vt.RecordType, field : vt.RecordField) extends RecordProcedure {
  override def cloningMap(f : Expr => Expr) : Expr =
    RecordMutator(recordType, field).assignLocationFrom(this)

  override def schemeType = vt.ProcedureType(
    fixedArgTypes=List(recordType, field.fieldType.schemeType),
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.SingleValue(vt.UnitType)
  )
}

case class TypePredicate(testingType : vt.SchemeType) extends ArtificialProcedure {
  override def cloningMap(f : Expr => Expr) : Expr =
    TypePredicate(testingType).assignLocationFrom(this)
}

/** Casts the result of an expression to a given type
  * 
  * @param  valueExpr    Expresion to have its result value cast
  * @param  targetType   Type to cast the the expression's result to  
  * @param  staticCheck  If true, the value must satisfy the target type at compile time. This is the mode used by
  *                      (ann)
  *                      If false, runtime type checks may be generated. This is the mode used by (cast).
  */
case class Cast(valueExpr : Expr, targetType : vt.SchemeType, staticCheck : Boolean) extends Expr {
  val subexprs = valueExpr :: Nil

  def map(f : Expr => Expr) : Cast =
    Cast(f(valueExpr), targetType, staticCheck).assignLocationFrom(this)

  override def schemeType = targetType
}

case class Parameterize(parameterValues : List[(Expr, Expr)], body : Expr) extends Expr {
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
case class Return(values : List[Expr]) extends Expr {
  lazy val subexprs = values

  def map(f : Expr => Expr) : Return = 
    Return(values.map(f)).assignLocationFrom(this)
}
