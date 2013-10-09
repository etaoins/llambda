package llambda.ast

import llambda.SchemeParserDefinitions

sealed abstract class Datum

// This helps out ScopedSyntaxTree by grouping all the types that have scope
// or contain datums with scope
sealed abstract class NonSymbolLeaf extends Datum

case object UnspecificValue extends NonSymbolLeaf {
  override def toString = "#!unspecific"
}

case class StringLiteral(content : String) extends NonSymbolLeaf {
  override def toString = '"' + content + '"'
}

case class BooleanLiteral(value : Boolean) extends NonSymbolLeaf {
  override def toString = value match {
    case true => "#t"
    case false => "#f"
  }
}

object TrueLiteral extends BooleanLiteral(true)
object FalseLiteral  extends BooleanLiteral(false)

sealed abstract class NumberLiteral extends NonSymbolLeaf 

case class IntegerLiteral(value : Int) extends NumberLiteral {
  override def toString = value.toString
}

case class RationalLiteral(value : Double) extends NumberLiteral {
  override def toString = value match {
    case Double.PositiveInfinity => "+inf.0"
    case Double.NegativeInfinity => "-inf.0"
    case nan if nan.isNaN        => "+nan.0"
    case _ => value.toString
  }
}

object PositiveInfinityLiteral extends RationalLiteral(Double.PositiveInfinity)
object NegativeInfinityLiteral extends RationalLiteral(Double.NegativeInfinity)
object NaNLiteral extends RationalLiteral(Double.NaN)

case class Symbol(name : String) extends Datum {
  override def toString = if (name.matches(SchemeParserDefinitions.identifierPattern)) {
    name
  }
  else {
    "|" + name.replace("|", "\\|") + "|"
  }
}

case object EmptyList extends NonSymbolLeaf {
  override def toString = "()"
}

case class Pair(car : Datum, cdr : Datum) extends Datum {
  override def toString = this match {
    case ProperList(data) =>
      "(" + data.mkString(" ") + ")"

    case ImproperList(head, terminator) =>
      "(" + head.mkString(" ") + " . " + terminator + ")"

    case _ =>
      // This isn't possible but the compiler can't prove that
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
      Some((List(car), cdr))
    case _ => None
  }

  def apply(head : List[Datum], terminator : Datum) = {
    head.foldRight(terminator : Datum) { (car, cdr) => Pair(car, cdr) }
  }
}

object ProperList {
  def unapply(datum : Datum) : Option[List[Datum]] = datum match {
    case EmptyList => Some(Nil)
    case Pair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }

  def apply(data : List[Datum]) : Datum = 
    data.foldRight(EmptyList : Datum) { (car, cdr) => Pair(car, cdr) }
}

case class VectorLiteral(elements : Vector[Datum]) extends Datum {
  override def toString = 
    "#(" + elements.map(_.toString).mkString(" ") + ")"
}

case class Bytevector(elements : Vector[Int]) extends NonSymbolLeaf {
  override def toString = 
    "#u8(" + elements.map(_.toString).mkString(" ") + ")"
}

case class CharLiteral(value : Char) extends NonSymbolLeaf {
  override def toString = value match {
    case '\0' => """#\null"""
    case ' '  => """#\space"""
    case '\n' => """#\newline"""
    case '\r' => """#\return"""
    case '\t' => """#\tab"""
    case _ if value.isLetterOrDigit && (value.toInt <= 127) =>
      """#\""" + value
    case _ =>
      """#\x""" + value.toInt.toHexString
  }
}
