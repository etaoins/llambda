package io.llambda.compiler.reducer.partialvalue
import io.llambda

import io.llambda.compiler._
import llambda.compiler.SourceLocated

/** Represents a partially evaluated value */
sealed abstract class PartialValue extends SourceLocated {
  /** Returns the datum representation of this value for constant data */
  def toDatumOpt : Option[ast.Datum]

  /** Returns the expression that produces this value for non-constant data 
    *
    * Some structural partial values such as partial pairs may have neither a datum or expression represenation.
    */
  def toExprOpt : Option[et.Expr]
}

/** Represents a partial value produced by a reduced expression */
case class ReducedExpr(expr : et.NonLiteralExpr) extends PartialValue {
  // This is a non-literal
  // By definition it has no known datum value
  def toDatumOpt : Option[ast.Datum] = None
  def toExprOpt = Some(expr)
}

/** Represents a partial value produced by constant data */
case class LiteralLeaf(literal : ast.Leaf) extends PartialValue {
  def toDatumOpt : Option[ast.Datum] =
    Some(literal)

  def toExprOpt =
    Some(et.Literal(literal).assignLocationFrom(this))
}

/** Represents a Pair of two partial values */
case class PartialPair(partialCar : PartialValue, partialCdr : PartialValue) extends PartialValue {
  def toDatumOpt : Option[ast.Datum] = {
    for(carExpr <- partialCar.toDatumOpt;
        cdrExpr <- partialCdr.toDatumOpt) yield ast.Pair(carExpr, cdrExpr).assignLocationFrom(this)
  }

  // We refuse to call (cons) to construct a pair
  def toExprOpt =
    toDatumOpt.map(et.Literal(_))
}

/** Represents a vector of known length of partial values */
case class PartialVector(partialElements : Vector[PartialValue]) extends PartialValue {
  def toDatumOpt : Option[ast.Datum] = {
    val elementExprs = partialElements.flatMap(_.toDatumOpt)

    // Were all of the elements converted?
    if (elementExprs.length == partialElements.length) {
      Some(
        ast.VectorLiteral(elementExprs).assignLocationFrom(this)
      )
    }
    else {
      None
    }
  }
  
  // We refuse to call (vector) to construct a vector
  def toExprOpt =
    toDatumOpt.map(et.Literal(_))
}

object ProperList {
  def unapply(partialValue : PartialValue) : Option[List[PartialValue]] = partialValue match {
    case LiteralLeaf(ast.EmptyList()) => Some(Nil)
    case PartialPair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }

  def apply(data : Seq[PartialValue]) : PartialValue = 
    data.foldRight(LiteralLeaf(ast.EmptyList()) : PartialValue) { (car, cdr) => 
      PartialPair(car, cdr) 
    }
}

object PartialValue {
  def fromDatum(datum : ast.Datum) : PartialValue = (datum match {
    case leafDatum : ast.Leaf =>
      LiteralLeaf(leafDatum)

    case ast.Pair(car, cdr) =>
      PartialPair(fromDatum(car), fromDatum(cdr))

    case ast.VectorLiteral(elements) =>
      PartialVector(elements.map(fromDatum))
  }).assignLocationFrom(datum)

  def fromReducedExpr(expr : et.Expr) = expr match {
    case nonLiteralExpr : et.NonLiteralExpr =>
      ReducedExpr(nonLiteralExpr)

    case et.Literal(literalDatum) =>
      PartialValue.fromDatum(literalDatum)
  }
}
