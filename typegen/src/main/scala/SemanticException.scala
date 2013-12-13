package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class SemanticException(val positional : Positional, message : String) extends 
  Exception(message + "\n" + positional.pos.longString) 

class DuplicateTypeNameException(val parsedDef : ParsedDefinition) extends
  SemanticException(parsedDef, s"Duplicate type name: ${parsedDef.name}")

class UndefinedCellClassException(val forwardDecl : ParsedCellClassDeclaration) extends
  SemanticException(forwardDecl, s"Undefined forward-declared cell class: ${forwardDecl.name}")

class UnknownTypeException(parsedTypeName : ParsedTypeName) extends
  SemanticException(parsedTypeName, s"Unknown type name: ${parsedTypeName.toString}")
