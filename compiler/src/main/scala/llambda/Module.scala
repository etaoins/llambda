package llambda

sealed abstract class ImportSet

case class LibraryImport(library : Library)  extends ImportSet

case class OnlyImport(importSet : ImportSet, identifiers : List[String])
case class ExceptImport(importSet : ImportSet, identifiers : List[String])
case class PrefixImport(importSet : ImportSet, prefix : String)
case class RenameImport(importSet : ImportSet, identifierMap : Map[String, String])

sealed abstract class Module(
    val imports : List[ImportSet],
    bindings : Map[String, BoundValue],
    val expressions : List[et.Expression])
  extends Scope(bindings)

class Program(
    imports : List[ImportSet],
    bindings : Map[String, BoundValue],
    expressions : List[et.Expression])
  extends Module(imports, bindings, expressions)

class Library(
    val name : List[ast.Datum],
    imports : List[ImportSet],
    val exports : Map[String, String],
    bindings : Map[String, BoundValue],
    expressions : List[et.Expression])
  extends Module(imports, bindings, expressions)
