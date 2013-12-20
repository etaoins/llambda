package io.llambda.typegen

import scala.util.parsing.input.{Position, Positional}

sealed abstract class ParsedType extends Positional

case class ParsedPointerType(pointeeType : ParsedType) extends ParsedType {
  override def toString = pointeeType + "*"

  // Only the outermost pointer is positioned by the parser
  // This is hack to make the pos propagate to the inner types
  override def setPos(newPos : Position) : this.type = {
    super.setPos(newPos)
    pointeeType.setPos(newPos)

    this
  }
}

case class ParsedFunctionPointerType(returnType : Option[ParsedType], arguments : List[ParsedType]) extends ParsedType {
  override def toString = s"""${returnType.getOrElse("void")} (*)(${arguments.mkString(", ")})"""
}

case class ParsedTypeName(name : String) extends ParsedType {
  override def toString = name
}
