package llambda

import llambda._

sealed abstract class SemanticException(message : String) extends Exception(message) {
  val semanticErrorType : String
}

// XXX: Make everything a LocatedSemanticException
sealed abstract class LocatedSemanticException(val located : SourceLocated, message : String) extends SemanticException(message + "\n" + located.locationString)

class DubiousLibraryNameComponentException(located : SourceLocated, val libraryName : String) extends LocatedSemanticException(located, libraryName) {
  val semanticErrorType = "dubious library name"
}

abstract class ReferencedFileNotFoundException(located : SourceLocated, val filename : String) extends LocatedSemanticException(located, filename)

class LibraryNotFoundException(located : SourceLocated, filename : String) extends ReferencedFileNotFoundException(located, filename) {
  val semanticErrorType = "library not found"
}

class IncludeNotFoundException(located : SourceLocated, filename : String) extends ReferencedFileNotFoundException(located, filename) {
  val semanticErrorType = "include not found"
}

class LibraryNameMismatchException(located : SourceLocated, val loadedName : Seq[LibraryNameComponent], val definedName : List[LibraryNameComponent]) extends
  LocatedSemanticException(located, "(" + loadedName.mkString(" ") + ") doesn't match (" + definedName.mkString(" ") + ")") {
  val semanticErrorType = "library name mismatch"
}

case class InvalidLibraryNameException(val datum : ast.Datum) extends LocatedSemanticException(datum, datum.toString) {
  val semanticErrorType = "invalid library name"
}

class NoSyntaxRuleException(located : SourceLocated, message : String) extends LocatedSemanticException(located, message) {
  val semanticErrorType = "no syntax rule"
}

class MalformedExpressionException(located : SourceLocated, message : String) extends LocatedSemanticException(located, message) {
  val semanticErrorType = "malformed expression"
}

class BadSpecialFormException(located : SourceLocated, message : String) extends LocatedSemanticException(located, message) {
  val semanticErrorType = "bad special form"
}

class UnboundVariableException(located : SourceLocated, val variableName : String) extends LocatedSemanticException(located, variableName) {
  val semanticErrorType = "unbound variable"
}

class UserDefinedSyntaxError(located : SourceLocated, val errorString  : String, val data : List[ast.Datum])
  extends LocatedSemanticException(located, errorString + " " + data.map(_.toString).mkString(" ")) {

  val semanticErrorType = "user defined syntax error"
}

class ImportedIdentifierNotFoundException(located : SourceLocated, val identifier : String) extends LocatedSemanticException(located, identifier) {
  val semanticErrorType = "imported identifier not found"
}

class ImpossibleTypeConversionException(message : String) extends SemanticException(message) {
  val semanticErrorType = "impossible type conversion"
}

class IncompatibleArityException(message : String) extends SemanticException(message) {
  val semanticErrorType = "incompatible arity"
}
