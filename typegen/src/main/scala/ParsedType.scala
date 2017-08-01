package io.llambda.typegen

import scala.util.parsing.input.{Position, Positional}

sealed abstract class ParsedType extends Positional

sealed abstract class ParsedIndirectionType extends ParsedType {
  val pointeeType: ParsedType

  // Only the outermost pointer is positioned by the parser
  // This is hack to make the pos propagate to the inner types
  override def setPos(newPos: Position): this.type = {
    super.setPos(newPos)
    pointeeType.setPos(newPos)

    this
  }
}

case class ParsedPointerType(pointeeType: ParsedType) extends ParsedIndirectionType {
  override def toString = pointeeType + "*"
}

case class ParsedArrayType(dimensions: List[Int], elementType: ParsedType) extends ParsedType {
  override def toString = elementType.toString + dimensions.map("[" + _.toString + "]")
}

case class ParsedFunctionPointerType(returnType: Option[ParsedType], arguments: List[ParsedType]) extends ParsedType {
  override def toString = s"""${returnType.getOrElse("void")} (*)(${arguments.mkString(", ")})"""
}

case class ParsedTypeName(name: String) extends ParsedType {
  override def toString = name
}
