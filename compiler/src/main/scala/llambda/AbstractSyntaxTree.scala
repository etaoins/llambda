package llambda.ast

import llambda._

sealed abstract class Datum {
  def truth = false

  def toProperList : Option[List[Datum]] = None
}

sealed abstract class Atom extends Datum

case class StringLiteral(name : String) extends Atom {
  override def toString = '"' + name + '"'
}

case class BooleanLiteral(value : Boolean) extends Atom {
  override def toString = value match {
    case true => "#t"
    case false => "#f"
  }
}

object TrueLiteral extends BooleanLiteral(true)
object FalseLiteral  extends BooleanLiteral(false)

sealed abstract class NumberLiteral extends Atom 

case class IntegerLiteral(value : Int) extends Atom {
  override def toString = value.toString
}

case class RealLiteral(value : Double) extends Atom {
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

case object EmptyList extends Atom {
  override def toString = "()"

  override def toProperList = Some(Nil)
}

case class Pair(car : Datum, cdr : Datum) extends Datum {
  override def toString = toProperList match {
      case Some(subdata) => "(" + subdata.map(_.toString).reduceLeft(_ + " " + _) + ")"
      case None => "(" + car.toString + " . " + cdr.toString + ")"
  }
  
  override def toProperList = cdr.toProperList.map(car :: _) 
}

object ProperList {
  def unapply(datum : Datum) = datum.toProperList

  def apply(data : Datum*) : Datum = 
    data.foldRight(EmptyList : Datum) { (car, cdr) => Pair(car, cdr) }
}

case class Vector(elements : Datum*) extends Atom {
  override def toString = 
    "#(" + elements.map(_.toString).mkString(" ") + ")"
}

case class ByteVector(elements : Int*) extends Atom {
  override def toString = 
    "#u8(" + elements.map(_.toString).mkString(" ") + ")"
}

case class CharLiteral(value : Char) extends Atom {
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
