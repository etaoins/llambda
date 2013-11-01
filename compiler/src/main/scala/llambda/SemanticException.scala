package llambda

import llambda._

sealed abstract class SemanticException(message : String) extends Exception(message) {
  val semanticErrorType : String
}

class DubiousLibraryNameComponentException(val libraryName : String) extends SemanticException(libraryName) {
  val semanticErrorType = "dubious library name"
}

abstract class ReferencedFileNotFoundException(val filename : String) extends SemanticException(filename)

class LibraryNotFoundException(filename : String) extends ReferencedFileNotFoundException(filename) {
  val semanticErrorType = "library not found"
}

class IncludeNotFoundException(filename : String) extends ReferencedFileNotFoundException(filename) {
  val semanticErrorType = "include not found"
}

class LibraryNameMismatchException(val loadedName : Seq[LibraryNameComponent], val definedName : List[LibraryNameComponent]) extends
  SemanticException(loadedName.mkString(" ") + " doesn't match " + definedName.mkString(" ")) {
  val semanticErrorType = "library name mismatch"
}

case class InvalidLibraryNameException(val datum : ast.Datum) extends SemanticException(datum.toString) {
  val semanticErrorType = "invalid library name"
}

class NoSyntaxRuleException(message : String) extends SemanticException(message) {
  val semanticErrorType = "no syntax rule"
}

class MalformedExpressionException(message : String) extends SemanticException(message) {
  val semanticErrorType = "malformed expression"
}

class BadSpecialFormException(message : String) extends SemanticException(message) {
  val semanticErrorType = "bad special form"
}

class UnboundVariableException(val variableName : String) extends SemanticException(variableName) {
  val semanticErrorType = "unbound variable"
}

class UserDefinedSyntaxError(val errorString  : String, val data : List[ast.Datum])
  extends SemanticException(errorString + " " + data.map(_.toString).mkString(" ")) {

  val semanticErrorType = "user defined syntax error"
}

class ImportedIdentifierNotFoundException(val identifier : String) extends SemanticException(identifier) {
  val semanticErrorType = "imported identifier not found"
}

class ImpossibleTypeConversionException(message : String) extends SemanticException(message) {
  val semanticErrorType = "impossible type conversion"
}

class IncompatibleArityException(message : String) extends SemanticException(message) {
  val semanticErrorType = "incompatible arity"
}
