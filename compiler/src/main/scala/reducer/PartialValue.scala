package io.llambda.compiler.reducer.partialvalue
import io.llambda

import io.llambda.compiler._
import llambda.compiler.SourceLocated

sealed abstract class PartialValue extends SourceLocated {
  def toDatumOpt : Option[ast.Datum]
}

case class ReducedExpression(expr : et.NonLiteralExpression) extends PartialValue {
  // This is a non-literal
  // By definition it has no known datum value
  def toDatumOpt : Option[ast.Datum] = None
}

case class LiteralLeaf(literal : ast.Leaf) extends PartialValue {
  def toDatumOpt : Option[ast.Datum] =
    Some(literal)
}

case class PartialPair(partialCar : PartialValue, partialCdr : PartialValue) extends PartialValue {
  def toDatumOpt : Option[ast.Datum] = {
    for(carExpr <- partialCar.toDatumOpt;
        cdrExpr <- partialCdr.toDatumOpt) yield ast.Pair(carExpr, cdrExpr).assignLocationFrom(this)
  }
}

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
}

object ProperList {
  def unapply(partialValue : PartialValue) : Option[List[PartialValue]] = partialValue match {
    case LiteralLeaf(ast.EmptyList()) => Some(Nil)
    case PartialPair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }

  def apply(data : List[PartialValue]) : PartialValue = 
    data.foldRight(LiteralLeaf(ast.EmptyList()) : PartialValue) { (car, cdr) => 
      PartialPair(car, cdr) 
    }
}

object PartialValue {
  def fromDatum(datum : ast.Datum) : PartialValue = (datum match {
    case leafDatum : ast.Leaf =>
      LiteralLeaf(leafDatum).assignLocationFrom(leafDatum)

    case ast.Pair(car, cdr) =>
      PartialPair(fromDatum(car), fromDatum(cdr))

    case ast.VectorLiteral(elements) =>
      PartialVector(elements.map(fromDatum))
  })

  def fromReducedExpression(expr : et.Expression) = expr match {
    case nonLiteralExpr : et.NonLiteralExpression =>
      ReducedExpression(nonLiteralExpr)

    case et.Literal(literalDatum) =>
      PartialValue.fromDatum(literalDatum)
  }
}
