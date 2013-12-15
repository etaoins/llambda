package io.llambda.typegen

import scala.util.parsing.input.Positional

sealed abstract class SemanticException(message : String) extends Exception(message) 

sealed abstract class PositionedSemanticException(val positional : Positional, message : String) extends 
  SemanticException(message + "\n" + positional.pos.longString) 

class DuplicateTypeNameException(val parsedDef : ParsedDefinition) extends
  PositionedSemanticException(parsedDef, s"Duplicate type name: ${parsedDef.name}")

class UndefinedCellClassException(errorPos : Positional, val cellClassName : String) extends
  PositionedSemanticException(errorPos, s"Undefined forward-declared cell class: ${cellClassName}")

class UnknownTypeException(val parsedTypeName : ParsedTypeName) extends
  PositionedSemanticException(parsedTypeName, s"Unknown type name: ${parsedTypeName.toString}")

class UndefinedTypeTagFieldException(val parsedDef : ParsedRootClassDefinition) 
  extends PositionedSemanticException(parsedDef, s"Undefined type tag field: ${parsedDef.typeTagField}")

class DuplicateRootCellClassException(val duplicateClass : RootCellClass) extends
  PositionedSemanticException(duplicateClass, s"Duplicate root cell class: ${duplicateClass.name}")

class NoRootCellClassException extends SemanticException("No root cell class defined")

class DuplicateFieldNameException(val parsedCellField : ParsedCellField) extends
  PositionedSemanticException(parsedCellField, s"Duplicate field name: ${parsedCellField.name}")

class ChildlessAbstractCellClassException(val cellClass : CellClass) extends
  PositionedSemanticException(cellClass, s"""Abstract cell class "${cellClass.name}" has no children""")

class NonAliasedTypeTagFieldException(val typeTagField : CellField) extends 
  PositionedSemanticException(typeTagField, s"Type tag field must have a type introduced by fieldtype")

class TypeTagAliasMissingCppNameException(val fieldTypeAlias : FieldTypeAlias) extends 
  PositionedSemanticException(fieldTypeAlias, s"Type tag field type alias must have a cppname")

class TypeTagAliasExternallyDefinedException(val fieldTypeAlias : FieldTypeAlias) extends 
  PositionedSemanticException(fieldTypeAlias, s"Type tag field type alias must not have an external definition")

class TypeTagAliasNonIntegralException(val fieldTypeAlias : FieldTypeAlias) extends 
  PositionedSemanticException(fieldTypeAlias, s"Type tag field type alias must have an integral type")
