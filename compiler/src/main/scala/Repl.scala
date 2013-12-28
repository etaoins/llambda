package io.llambda.compiler
import io.llambda

import scala.tools.jline.console.{ConsoleReader, history}
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
class BodyExpressionMode(targetPlatform : platform.TargetPlatform) extends SchemeParsingMode("expr") {
  private val loader = new frontend.LibraryLoader(targetPlatform)
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
class CompileMode(targetPlatform : platform.TargetPlatform) extends SchemeParsingMode("compile") {
  val compileConfig = CompileConfig(
    includePath=ReplIncludePath(),
    optimizeLevel=2,
    targetPlatform=targetPlatform)

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
class Repl(targetPlatform : platform.TargetPlatform) {
  @tailrec
  private def acceptInput(mode : ReplMode)(implicit reader : ConsoleReader) {
    val command = reader.readLine(mode.name + "> ")

    // Flush the last command to our history file
    reader.getHistory match {
      case flushable : history.PersistentHistory =>
        flushable.flush()
    }

    command match {
      case ":quit" =>
        return;

      case ":parse" =>
        acceptInput(new ParseOnlyMode)
      
      case ":expr" =>
        acceptInput(new BodyExpressionMode(targetPlatform))
      
      case ":compile" =>
        acceptInput(new CompileMode(targetPlatform))

      case userString =>
        mode.evaluate(userString)
        acceptInput(mode)
    }
  }

  def apply() {
    // Get our history file
    val llambdaDir = new File(System.getProperty("user.home"), ".llambda")
    llambdaDir.mkdir()
    val historyFile = new File(llambdaDir, "repl-history")

    val reader = new ConsoleReader;
    reader.setHistory(new history.FileHistory(historyFile))

    acceptInput(new CompileMode(targetPlatform))(reader)
  }
}
