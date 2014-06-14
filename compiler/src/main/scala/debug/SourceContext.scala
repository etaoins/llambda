package io.llambda.compiler.debug
import io.llambda

/** Represents a DWARF source context
  *
  * This is used to generate debugging information
  */
sealed abstract class SourceContext

/** Represents an unknown context
  *
  * This is intended for use by the REPL and unit tests
  */
case object UnknownContext extends SourceContext

/** Represents the top-level context of a file
  *
  * This is a case class because top-level subprograms are defined entirely by their filename 
  */
case class FileContext(filename : String) extends SourceContext

/** Represents a subprogram (context) procedure in the Scheme source 
  *
  * This is not a case class because the source procedures filename, start line and source name aren't necessarily
  * unique.
  */
class SubprogramContext(val parentContext : SourceContext, val filenameOpt : Option[String], val startLine : Int, val sourceNameOpt : Option[String]) extends SourceContext

object SourceContext {
  def fromFilenameOpt(filenameOpt : Option[String]) =
    filenameOpt.map(FileContext(_)).getOrElse(UnknownContext)
}
