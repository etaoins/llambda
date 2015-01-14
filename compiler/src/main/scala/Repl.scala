package io.llambda.compiler
import io.llambda

import java.io.{InputStream, File}

import scala.io.Source
import scala.sys.process._
import scala.collection.mutable.ListBuffer
import annotation.tailrec

import jline.console.{ConsoleReader, history}

class ReplProcessNonZeroExitException(val code : Int, val output : String) extends Exception(s"Process exited with code ${code}. Output:\n${output}")

private object ReplFrontendConfig {
  def apply(targetPlatform : platform.TargetPlatform, schemeDialect : dialect.Dialect) : frontend.FrontendConfig =  {
    val currentDirUrl = (new File(System.getProperty("user.dir"))).toURI.toURL

    val includePath = frontend.IncludePath(
      fileParentDir=Some(currentDirUrl),
      packageRootDir=Some(currentDirUrl)
    )

    frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=FeatureIdentifiers(targetPlatform, schemeDialect),
      schemeDialect=schemeDialect
    )
  }
}

/** Implements the REPL loop and switching modes */
class Repl(targetPlatform : platform.TargetPlatform, schemeDialect : dialect.Dialect) {
  private val frontendConfig = ReplFrontendConfig(targetPlatform, schemeDialect)
  private val compileConfig = CompileConfig(
    includePath=frontendConfig.includePath,
    optimiseLevel=2,
    targetPlatform=targetPlatform,
    schemeDialect=schemeDialect
  )

  private val loader = new frontend.LibraryLoader(targetPlatform)
  private val importDecls = new ListBuffer[ast.Datum]

  importDecls +=
    ast.ProperList(List(
      ast.Symbol("import"),
      ast.ProperList(List(
        ast.Symbol("scheme"),
        ast.Symbol("base")
      )),
      ast.ProperList(List(
        ast.Symbol("scheme"),
        ast.Symbol("write")
      ))
    ))

  def evalDatum(userDatum : ast.Datum) : String = userDatum match {
    case importDecl @ ast.ProperList(ast.Symbol("import") :: _) =>
      // Make sure this exists up front
      frontend.ResolveImportDecl(importDecl)(loader, frontendConfig)

      importDecls += importDecl
      "loaded"

    case _ =>
      val outputFile = File.createTempFile("llambdarepl", null, null)
      outputFile.deleteOnExit()

      // Implicitly import (scheme base) and wrap in (write)
      // This is encouraged for REPLs by R7RS
      val programData = importDecls.toList :+
        ast.ProperList(List(
          ast.Symbol("write"),
          userDatum
        ))

      try {
        Compiler.compileData(programData, outputFile, compileConfig)

        // Create our output logger
        var stdout : Option[InputStream] = None

        val outputIO = new ProcessIO(
          stdin  => Unit, // Don't care
          stdoutStream => stdout = Some(stdoutStream),
          BasicIO.toStdErr
        )

        // Run and capture the output
        val process = Process(outputFile.getAbsolutePath).run(outputIO)

        val exitValue = process.exitValue()
        val outputString = Source.fromInputStream(stdout.get, "UTF-8").mkString

        if (process.exitValue() != 0) {
          throw new ReplProcessNonZeroExitException(process.exitValue(), outputString)
        }

        outputString
      }
      finally {
        outputFile.delete()
      }
  }

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

    println()
  }

  @tailrec
  private def acceptInput()(implicit reader : ConsoleReader) {
    val command = reader.readLine("llambda> ")

    // Flush the last command to our history file
    reader.getHistory match {
      case flushable : history.PersistentHistory =>
        flushable.flush()
    }

    command match {
      case ":quit" =>
        return;

      case userString =>
        evaluate(userString)
        acceptInput()
    }
  }

  def apply() {
    // Get our history file
    val llambdaDir = new File(System.getProperty("user.home"), ".llambda")
    llambdaDir.mkdir()
    val historyFile = new File(llambdaDir, "repl-history")

    val reader = new ConsoleReader;
    reader.setHistory(new history.FileHistory(historyFile))

    acceptInput()(reader)
  }
}
