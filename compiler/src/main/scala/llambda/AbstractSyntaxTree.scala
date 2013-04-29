package llambda.ast

import llambda._

sealed abstract class Datum
sealed abstract class Atom extends Datum

// This helps out ScopedSyntaxTree by grouping all the types that don't need scope
sealed abstract class NonSymbolAtom extends Atom

case class StringLiteral(name : String) extends NonSymbolAtom {
  override def toString = '"' + name + '"'
}

case class BooleanLiteral(value : Boolean) extends NonSymbolAtom {
  override def toString = value match {
    case true => "#t"
    case false => "#f"
  }
}

object TrueLiteral extends BooleanLiteral(true)
object FalseLiteral  extends BooleanLiteral(false)

sealed abstract class NumberLiteral extends NonSymbolAtom 

case class IntegerLiteral(value : Int) extends NumberLiteral {
  override def toString = value.toString
}

case class RealLiteral(value : Double) extends NumberLiteral {
  override def toString = value.toString
}

object PositiveInfinityLiteral extends RealLiteral(Double.PositiveInfinity)
object NegativeInfinityLiteral extends RealLiteral(Double.NegativeInfinity)
object NaNLiteral extends RealLiteral(Double.NaN)

case class Symbol(name : String) extends Atom {
  override def toString = if (name.matches(SchemeParserDefinitions.identifierPattern)) {
    name
  }
  else {
    "|" + name + "|"
  }
}

case object EmptyList extends NonSymbolAtom {
  override def toString = "()"
}

case class Pair(car : Datum, cdr : Datum) extends Datum {
  override def toString = this match {
    case ProperList(data) =>
      "(" + data.mkString(" ") + ")"

    case ImproperList(head, terminator) =>
      "(" + head.mkString(" ") + " . " + terminator + ")"

    case _ =>
      // This isn't possible but the compile can't prove that
      "(" + car + " . " + cdr + ")"
  }
}

object ImproperList {
  def unapply(datum : Datum) : Option[(List[Datum], Datum)] = datum match {
    case Pair(_, EmptyList) => None // This is a proper list
    case Pair(car, tail : Pair)  => 
      ImproperList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case Pair(car, cdr) =>
      // This is the end of an improper list
      Some(List(car), cdr)
    case _ => None
  }

  def apply(head : List[Datum], terminator : Datum) = {
    head.foldRight(terminator : ast.Datum) { (car, cdr) => Pair(car, cdr) }
  }
}

object ProperList {
  def unapply(datum : Datum) : Option[List[Datum]] = datum match {
    case EmptyList => Some(Nil)
    case Pair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }

  def apply(data : Datum*) : Datum = 
    data.foldRight(EmptyList : Datum) { (car, cdr) => Pair(car, cdr) }
}

case class Vector(elements : Datum*) extends NonSymbolAtom {
  override def toString = 
    "#(" + elements.map(_.toString).mkString(" ") + ")"
}

case class ByteVector(elements : Int*) extends NonSymbolAtom {
  override def toString = 
    "#u8(" + elements.map(_.toString).mkString(" ") + ")"
}

case class CharLiteral(value : Char) extends NonSymbolAtom {
  override def toString = value match {
    case '\0' => """\#null"""
    case ' '  => """\#space"""
    case '\n' => """\#newline"""
    case '\r' => """\#return"""
    case '\t' => """\#tab"""
    case _ if value.isLetterOrDigit && (value.toInt <= 127) =>
      """\#""" + value
    case _ =>
      """\#x""" + value.toInt.toHexString
  }
}
