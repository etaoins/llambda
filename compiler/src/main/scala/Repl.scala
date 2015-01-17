package io.llambda.compiler
import io.llambda

import java.io.{InputStream, File}

import scala.io.Source
import scala.sys.process._
import scala.collection.mutable
import scala.collection.JavaConversions._
import annotation.tailrec

import jline.console.{ConsoleReader, history, completer}

class ReplProcessNonZeroExitException(val code : Int, val stdout : String, val stderr : String) extends
  Exception(s"Process exited with code ${code}")

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
          other.sorted
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
  ).map({ strings =>
    strings.map(StringComponent(_))
  }) ++ frontendConfig.schemeDialect.implicitLibraryNames

  val initialBindings = initialLibraries.flatMap(loader.load(_))

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

  // ANSI colour codes
  val failureColour = 31
  val successColour = 32

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

  // Don't expand events - ! is used frequently in Scheme identifiers
  reader.setExpandEvents(false)

  private def setCompleter(newCompleter : completer.Completer) : Unit = {
    for(completer <- reader.getCompleters.toList) {
      reader.removeCompleter(completer)
    }

    reader.addCompleter(newCompleter)
  }

  def colourStr(str : String, colourCode : Int) =
    s"\u001B[${colourCode}m${str}\u001B[0m"

  private def exprsToOutputString(exprs : List[et.Expr]) : String = {
    val outputFile = File.createTempFile("llambdarepl", null, null)
    outputFile.deleteOnExit()

    try {
      Compiler.compileExprs(exprs, outputFile, compileConfig, None)

      // Create our output streams
      var stdout : Option[InputStream] = None
      var stderr : Option[InputStream] = None

      val outputIO = new ProcessIO(
        stdin  => Unit, // Don't care
        stdoutStream => stdout = Some(stdoutStream),
        stderrStream => stderr = Some(stderrStream)
      )

      // Run and capture the output
      val process = Process(outputFile.getAbsolutePath).run(outputIO)

      val exitValue = process.exitValue()

      val stdoutString = Source.fromInputStream(stdout.get, "UTF-8").mkString
      val stderrString = Source.fromInputStream(stderr.get, "UTF-8").mkString

      if (exitValue != 0) {
        throw new ReplProcessNonZeroExitException(exitValue, stdoutString, stderrString)
      }

      stdoutString
    }
    finally {
      outputFile.delete()
    }
  }

  /** Handles evaluating definitions that only produce abstract bindings
    *
    * This includes (define-type) and (define-syntax)
    */
  def evalSimpleDefine(defineDatum : ast.Datum) : String = {
    // Evaluating this in our top-level scope and collect any expressions required for record type defines
    state.prefixExprs ++= state.extractor(List(defineDatum), state.scope)

    "defined"
  }

  /** Handles evaluating definitions of values and mutations */
  def evalValueDefine(defineDatum : ast.Datum, identifier : String) : String = {
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

    val memoizedExprsOpt = try {
      val resultData = SchemeParser.parseStringAsData(resultString)

      resultData match {
        case List(resultDatum) =>
          // We can replace this with a static datum. This means we don't have to re-evaluate it on every REPL program
          //  and unstable values are fixed.
          defineExprs match {
            case List(et.TopLevelDefine(List(et.SingleBinding(storageLoc, _)))) =>
              Some(List(et.TopLevelDefine(List(et.SingleBinding(storageLoc, et.Literal(resultDatum))))))

            case List(et.MutateVar(storageLoc, _)) =>
              Some(List(et.MutateVar(storageLoc, et.Literal(resultDatum))))

            case _ =>
              None
          }

        case _ =>
          None
      }
    }
    catch {
      case _ : ParseErrorException =>
        None
    }

    val newPrefixExprs = memoizedExprsOpt.getOrElse(defineExprs)

    // Save this if the program worked correctly
    state.prefixExprs ++= newPrefixExprs
    state.scope = childScope

    // Use a distinct separator for memoized results versus recalculated results
    val separator = if (memoizedExprsOpt.isDefined) "=>" else "->"

    s"${identifier} ${separator} ${resultString}"
  }

  def evalDatum(userDatum : ast.Datum) : String = userDatum match {
    case importDecl @ ast.ProperList(ast.Symbol("import") :: _) =>
      // Load the library
      state.scope.bindings ++= frontend.ResolveImportDecl(importDecl)(state.loader, frontendConfig)

      "loaded"

    // Simple (define)
    case defineDatum @ ast.ProperList(ast.Symbol(defineIdent) :: ast.Symbol(identifier) :: _)
        if state.scope.get(defineIdent) == Some(Primitives.Define) =>
      evalValueDefine(defineDatum, identifier)

    // Lambda shorthand
    case defineDatum @ ast.ProperList(ast.Symbol(defineIdent) :: ast.ProperList(ast.Symbol(identifier) :: _) :: _)
        if state.scope.get(defineIdent) == Some(Primitives.Define) =>
      evalValueDefine(defineDatum, identifier)

    // (set!)
    case defineDatum @ ast.ProperList(ast.Symbol(setIdent) :: ast.Symbol(identifier) :: _)
        if state.scope.get(setIdent) == Some(Primitives.Set) =>
      evalValueDefine(defineDatum, identifier)

    // (define-syntax)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if state.scope.get(defineTypeIdent) == Some(Primitives.DefineSyntax) =>
      evalSimpleDefine(defineDatum)

    // (define-type)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if state.scope.get(defineTypeIdent) == Some(Primitives.DefineType) =>
      evalSimpleDefine(defineDatum)

    // (define-record-type)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if state.scope.get(defineTypeIdent) == Some(Primitives.DefineRecordType) =>
      evalSimpleDefine(defineDatum)

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

      val caseFoldedData = if (schemeDialect.caseFoldPrograms) {
        data.map(_.toCaseFolded)
      }
      else {
        data
      }

      for(datum <- caseFoldedData) {
        println(colourStr("res: ", successColour) + evalDatum(datum))
      }
    }
    catch {
      case semantic : SemanticException =>
        val colouredError = colourStr(semantic.semanticErrorType, failureColour)
        println(s"${colouredError}: ${semantic.getMessage}")

      case parse : ParseErrorException =>
        println(colourStr("parse error: ", failureColour) + parse.getMessage)

      case nonzero : ReplProcessNonZeroExitException  =>
        if (!nonzero.stdout.isEmpty) {
          println(s"${nonzero.stdout}")
        }

        if (!nonzero.stderr.isEmpty) {
          println(s"${nonzero.stderr}")
        }

        println(colourStr("non-zero exit: ", failureColour) + nonzero.getMessage)
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

      case ":help" =>
        println("Special commands:")
        println(":reset\t\tResets the REPL state including definitions and imported libraries")
        println(":quit \t\tQuits the REPL")
        println(":help \t\tPrints this help")
        println()
        println("All other input is treated as a top-level Scheme expression")

        acceptInput()

      case userString =>
        evaluate(userString)
        acceptInput()
    }
  }

  def apply() {
    acceptInput()(reader)
    reader.getTerminal.restore()
  }
}
