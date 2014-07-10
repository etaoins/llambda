package io.llambda.compiler.ast
import io.llambda

import llambda.compiler.SchemeParser
import llambda.compiler.SourceLocated
import llambda.compiler.{valuetype => vt}

sealed abstract class Datum extends SourceLocated {
  /** Scheme type for this datum */
  val schemeType : vt.NonUnionSchemeType
}
 
sealed abstract class Leaf extends Datum

// This helps out ScopedSyntaxTree by grouping all the types that have scope or contain datums with scope
sealed abstract class NonSymbolLeaf extends Leaf

case class UnitValue() extends NonSymbolLeaf {
  val schemeType = vt.UnitType
  override def toString = "#!unit"
}

case class StringLiteral(content : String) extends NonSymbolLeaf {
  val schemeType = vt.StringType

  private def escapedContent = 
    content.replaceAllLiterally("\\", "\\" + "\\")
           .replaceAllLiterally("\"", "\\" + "\"")

  override def toString =
    '"' + escapedContent + '"'
}

case class BooleanLiteral(value : Boolean) extends NonSymbolLeaf {
  val schemeType = vt.ConstantBooleanType(value)

  override def toString = value match {
    case true => "#t"
    case false => "#f"
  }
}

sealed abstract class NumberLiteral extends NonSymbolLeaf 

case class IntegerLiteral(value : Long) extends NumberLiteral {
  val schemeType = vt.ExactIntegerType

  override def toString = value.toString
}

case class RationalLiteral(value : Double) extends NumberLiteral {
  val schemeType = vt.InexactRationalType
  
  // Consider all NaN literals to be equal
  // This is different from numeric equality which indeed doesn't make sense for NaNs
  override def equals(other : Any) : Boolean = other match {
    case RationalLiteral(otherValue) =>
      if (otherValue.isNaN) {
        value.isNaN
      }
      else {
        value == otherValue
      }

    case _ =>
      false
  }

  override def toString = value match {
    case Double.PositiveInfinity => "+inf.0"
    case Double.NegativeInfinity => "-inf.0"
    case nan if nan.isNaN        => "+nan.0"
    case _ => value.toString
  }
}

object PositiveInfinityLiteral {
  def apply() = RationalLiteral(Double.PositiveInfinity)
}

object NegativeInfinityLiteral {
  def apply() = RationalLiteral(Double.NegativeInfinity)
}

object NaNLiteral {
  def apply() =  RationalLiteral(Double.NaN)
}

case class Symbol(name : String) extends Leaf {
  val schemeType = vt.SymbolType

  override def toString = if (SchemeParser.isValidIdentifier(name)) {
    name
  }
  else {
    "|" + name.replace("|", "\\|") + "|"
  }
}

case class EmptyList() extends NonSymbolLeaf {
  val schemeType = vt.EmptyListType

  override def toString = "()"
}

case class Pair(car : Datum, cdr : Datum) extends Datum {
  val schemeType = vt.PairType

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

object AnyList {
  def unapply(datum : Datum) : Option[(List[Datum], Datum)] = datum match {
    case Pair(car, tail : Pair)  => 
      AnyList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case Pair(car, cdr) =>
      // This is the end of an improper list
      Some((List(car), cdr))
    case _ => None
  }

  def apply(head : Seq[Datum], terminator : Datum) = {
    head.foldRight(terminator : Datum) { (car, cdr) => Pair(car, cdr) }
  }
}

object ImproperList {
  def unapply(datum : Datum) : Option[(List[Datum], Datum)] = datum match {
    case Pair(_, EmptyList()) => None // This is a proper list
    case Pair(car, tail : Pair)  => 
      ImproperList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case Pair(car, cdr) =>
      // This is the end of an improper list
      Some((List(car), cdr))
    case _ => None
  }
}

object ProperList {
  def unapply(datum : Datum) : Option[List[Datum]] = datum match {
    case EmptyList() => Some(Nil)
    case Pair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }

  def apply(data : Seq[Datum]) : Datum = 
    data.foldRight(EmptyList() : Datum) { (car, cdr) => Pair(car, cdr) }
}

case class VectorLiteral(elements : Vector[Datum]) extends Datum {
  val schemeType = vt.VectorType

  override def toString = 
    "#(" + elements.map(_.toString).mkString(" ") + ")"
}

case class Bytevector(elements : Vector[Short]) extends NonSymbolLeaf {
  val schemeType = vt.BytevectorType

  override def toString = 
    "#u8(" + elements.map(_.toString).mkString(" ") + ")"
}

case class CharLiteral(value : Char) extends NonSymbolLeaf {
  val schemeType = vt.CharacterType

  override def toString = value match {
    case 0    => """#\null"""
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
