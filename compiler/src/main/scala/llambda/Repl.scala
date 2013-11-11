package llambda

import scala.tools.jline.console.ConsoleReader
import annotation.tailrec
import java.io.File

/** Base class for all REPL modes */
abstract class ReplMode(val name : String) {
  def evaluate(input : String) : Unit
}

/** Base class for REPL modes that involve parsing scheme */
abstract class SchemeParsingMode(name : String) extends ReplMode(name) {
  def evalDatum(data : ast.Datum) : String

  def evaluate(userString : String) {
    try {
      val data = SchemeParser.parseStringAsData(userString, Some("input")) 

      for(datum <- data) {
        println("res: " + evalDatum(datum))
      }
    }
    catch {
      case semantic : SemanticException =>
        println(s"${semantic.semanticErrorType}: ${semantic.getMessage}")
      
      case parse : ParseErrorException =>
        println("parse error: " + parse)
    }
  }
}

/** Just parses Scheme */
class ParseOnlyMode extends SchemeParsingMode("parse") {
  def evalDatum(datum : ast.Datum) : String =
    datum.toString
}

/** Extract expressions allowed in a library, program or lambda body */
class BodyExpressionMode extends SchemeParsingMode("body") {
  private implicit val loader = new frontend.LibraryLoader
  private val schemeBaseBindings = loader.loadSchemeBase

  implicit val scope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

  // Make our include path with the current directory
  val currentDirUrl = (new File(System.getProperty("user.dir"))).toURI.toURL
  implicit val includePath = frontend.IncludePath(
    fileParentDir=Some(currentDirUrl),
    packageRootDir=Some(currentDirUrl)
  )

  def evalDatum(datum : ast.Datum) = {
    datum match {
      case ast.ProperList(ast.Symbol("import") :: _) =>


        // This is an import decl - import our new bindings
        val newBindings = frontend.ResolveImportDecl(datum)(loader, includePath)

        scope ++= newBindings

        "loaded"
      case _ =>
        // Treat this like a body expression
        frontend.ExtractModuleBody(datum :: Nil).map(_.toString).mkString(" ")
    }
  }
}

/** Implements the REPL loop and switching modes */
object Repl {
  @tailrec
  private def acceptInput(mode : ReplMode)(implicit reader : ConsoleReader) {
    val command = reader.readLine(mode.name + "> ")

    command match {
      case ":quit" =>
        return;

      case ":parse" =>
        acceptInput(new ParseOnlyMode)
      
      case ":body" =>
        acceptInput(new BodyExpressionMode)

      case userString =>
        mode.evaluate(userString)
        acceptInput(mode)
    }
  }

  def apply() {
    val reader = new ConsoleReader;
    acceptInput(new BodyExpressionMode)(reader)
  }
}
