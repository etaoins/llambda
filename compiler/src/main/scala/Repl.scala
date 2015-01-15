package io.llambda.compiler
import io.llambda

import java.io.{InputStream, File}

import scala.io.Source
import scala.sys.process._
import scala.collection.mutable
import scala.collection.JavaConversions._
import annotation.tailrec

import jline.console.{ConsoleReader, history, completer}

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

/** Tab completion based on the current scope */
class ScopeCompleter(scope : Scope) extends completer.Completer {
  private def identifierStart(buffer : String, start : Int, end : Int) : Int = {
    if (start > 0) {
      val newStart = start - 1
      val testIdent = buffer.drop(newStart).take(end - newStart)

      if (SchemeParser.isValidIdentifier(testIdent)) {
        // There's more identifier to find
        identifierStart(buffer,newStart, end)
      }
      else {
        // Ran out of identifier
        start
      }
    }
    else {
      // We reached the beginning
      0
    }
  }

  def complete(buffer : String, cursor : Int, candidates : java.util.List[CharSequence]) : Int = {
    if (buffer == null) {
      candidates.addAll(scope.bindings.keys)
      0
    }
    else {
      val identStart = identifierStart(buffer, cursor, cursor)
      val identEnd = cursor

      val ident = buffer.drop(identStart).take(identEnd - identStart)
      val rawCompletions = scope.bindings.keys.filter(_.startsWith(ident)).toList

      val completions = rawCompletions.toList match {
        case List(single) =>
          // This is unambiguous - add a space after to tell the user we're done
          List(single + " ")

        case other =>
          other
      }

      candidates.addAll(completions)
      identStart
    }
  }
}

/** Class representing the current state of the REPL */
class ReplState(targetPlatform : platform.TargetPlatform, implicit val frontendConfig : frontend.FrontendConfig) {
  val loader : frontend.LibraryLoader = new frontend.LibraryLoader(targetPlatform)
  val prefixExprs : mutable.ListBuffer[et.Expr] = new mutable.ListBuffer
  val extractor = new frontend.ModuleBodyExtractor(debug.UnknownContext, loader, frontendConfig)

  val initialLibraries = List(
    List("scheme", "base"),
    List("scheme", "write"),
    List("llambda", "internal", "repl")
  )

  val initialBindings = initialLibraries flatMap { stringComponents =>
    loader.load(stringComponents.map(StringComponent(_)))
  }

  var scope : Scope = new Scope(mutable.Map(initialBindings : _*))
}

/** Implements the REPL loop and switching modes */
class Repl(targetPlatform : platform.TargetPlatform, schemeDialect : dialect.Dialect) {
  private implicit val frontendConfig = ReplFrontendConfig(targetPlatform, schemeDialect)

  private val compileConfig = CompileConfig(
    includePath=frontendConfig.includePath,
    optimiseLevel=2,
    targetPlatform=targetPlatform,
    schemeDialect=schemeDialect
  )

  private var state = new ReplState(targetPlatform, frontendConfig)

  // Create our reader
  val reader = new ConsoleReader;

  // Make our history dir
  val llambdaDir = new File(System.getProperty("user.home"), ".llambda")
  llambdaDir.mkdir()

  // Set our history file
  val historyFile = new File(llambdaDir, "repl-history")
  reader.setHistory(new history.FileHistory(historyFile))

  // Update our initial auto-completion set
  setCompleter(new ScopeCompleter(state.scope))

  private def setCompleter(newCompleter : completer.Completer) : Unit = {
    for(completer <- reader.getCompleters.toList) {
      reader.removeCompleter(completer)
    }

    reader.addCompleter(newCompleter)
  }

  private def exprsToOutputString(exprs : List[et.Expr]) : String = {
    val outputFile = File.createTempFile("llambdarepl", null, null)
    outputFile.deleteOnExit()

    try {
      Compiler.compileExprs(exprs, outputFile, compileConfig, None)

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

      if (exitValue != 0) {
        throw new ReplProcessNonZeroExitException(exitValue, outputString)
      }

      outputString
    }
    finally {
      outputFile.delete()
    }
  }

  def evalDatum(userDatum : ast.Datum) : String = userDatum match {
    case importDecl @ ast.ProperList(ast.Symbol("import") :: _) =>
      // Load the library
      state.scope.bindings ++= frontend.ResolveImportDecl(importDecl)(state.loader, frontendConfig)

      "loaded"

    case defineDatum @ ast.ProperList(List(ast.Symbol("define"), ast.Symbol(identifier), _)) =>
      // Create a test scope in case binding fails
      val childScope = new Scope(mutable.Map(), Some(state.scope))

      // Bind the value using the original (define)
      val defineExprs = state.extractor(List(defineDatum), childScope)

      // Then print the bound datum
      val printingDatum =
        ast.ProperList(List(
          ast.Symbol("write"),
          ast.Symbol(identifier)
        ))

      val printingExprs = state.extractor(List(printingDatum), childScope)
      val programExprs = state.loader.libraryExprs ++ state.prefixExprs.toList ++ defineExprs ++ printingExprs

      val resultString = exprsToOutputString(programExprs)

      // Save this if the program worked correctly
      state.prefixExprs ++= defineExprs
      state.scope = childScope

      identifier + " => " + resultString

    case _ =>
      val printingDatum =
        ast.ProperList(List(
          ast.Symbol("print-thunk-result"),
          ast.ProperList(List(
            ast.Symbol("lambda"),
            ast.ProperList(Nil),
            userDatum
          ))
        ))

      val printingExprs = state.extractor(List(printingDatum), state.scope)
      val programExprs = state.loader.libraryExprs ++ state.prefixExprs.toList ++ printingExprs

      exprsToOutputString(programExprs)
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
      case ":reset" =>
        // Replace our mutable state
        state = new ReplState(targetPlatform, frontendConfig)
        setCompleter(new ScopeCompleter(state.scope))

        acceptInput()

      case ":quit" =>
        return;

      case userString =>
        evaluate(userString)
        acceptInput()
    }
  }

  def apply() {
    acceptInput()(reader)
  }
}
