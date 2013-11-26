package llambda

import scala.tools.jline.console.ConsoleReader
import scala.sys.process._
import annotation.tailrec
import java.io.File

class ReplProcessNonZeroExitException(val code : Int, val output : String) extends Exception(s"Process exited with code ${code}. Output:\n${output}")

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
        println("parse error: " + parse.getMessage)
      
      case nonzero : ReplProcessNonZeroExitException  =>
        println("non-zero exit: " + nonzero.getMessage)
    }
  }
}

/** Provides an include path with the current directory */
object ReplIncludePath {
  def apply() : frontend.IncludePath =  {
    val currentDirUrl = (new File(System.getProperty("user.dir"))).toURI.toURL

    frontend.IncludePath(
      fileParentDir=Some(currentDirUrl),
      packageRootDir=Some(currentDirUrl)
    )
  }
}

/** Just parses Scheme */
class ParseOnlyMode extends SchemeParsingMode("parse") {
  def evalDatum(datum : ast.Datum) : String =
    datum.toString
}

/** Extract expressions allowed in a library, program or lambda body */
class BodyExpressionMode extends SchemeParsingMode("expr") {
  private val loader = new frontend.LibraryLoader
  private val schemeBaseBindings = loader.loadSchemeBase

  val scope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))
  val includePath = ReplIncludePath()

  val bodyExtractor = new frontend.ModuleBodyExtractor(loader, includePath)

  def evalDatum(datum : ast.Datum) = {
    datum match {
      case ast.ProperList(ast.Symbol("import") :: _) =>
        // This is an import decl - import our new bindings
        val newBindings = frontend.ResolveImportDecl(datum)(loader, includePath)

        scope ++= newBindings

        "loaded"
      case _ =>
        // Treat this like a body expression
        bodyExtractor(datum :: Nil, scope).map(_.toString).mkString(" ")
    }
  }
}

/** Compiles expressions as a standalone program and executes them */
class CompileMode extends SchemeParsingMode("compile") {
  val compileConfig = CompileConfig(
    includePath=ReplIncludePath(),
    optimizeLevel=2)

  def evalDatum(userDatum : ast.Datum) = {
    val outputFile = File.createTempFile("llambdarepl", null, null)
    outputFile.deleteOnExit()

    // Implicitly import (scheme base) and wrap in (write)
    // This is encouraged for REPLs by R7RS
    val programData = ast.ProperList(List(
      ast.Symbol("import"),
      ast.ProperList(List(
        ast.Symbol("scheme"),
        ast.Symbol("base")
      )),
      ast.ProperList(List(
        ast.Symbol("scheme"),
        ast.Symbol("write")
      ))
    )) :: ast.ProperList(List(
      ast.Symbol("write"),
      userDatum
    )) :: Nil

    try {
      Compiler.compileData(programData, outputFile, compileConfig)

      var outputString = ""
      val outLogger = ProcessLogger(line => outputString += line + "\n",
                                    line => outputString += line + "\n")

      // Run and capture the output
      val process = Process(outputFile.getAbsolutePath).run(outLogger)

      if (process.exitValue() != 0) {
        throw new ReplProcessNonZeroExitException(process.exitValue(), outputString)
      }

      outputString

    }
    finally {
      outputFile.delete()
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
      
      case ":expr" =>
        acceptInput(new BodyExpressionMode)
      
      case ":compile" =>
        acceptInput(new CompileMode)

      case userString =>
        mode.evaluate(userString)
        acceptInput(mode)
    }
  }

  def apply() {
    val reader = new ConsoleReader;
    acceptInput(new CompileMode)(reader)
  }
}
