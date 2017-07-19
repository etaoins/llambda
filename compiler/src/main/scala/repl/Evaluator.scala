package io.llambda.compiler.repl
import io.llambda

import llambda.compiler._

import java.io.File
import scala.collection.mutable


class ReplProcessNonZeroExitException(val code: Int, val stdout: String, val stderr: String) extends
  Exception(s"Process exited with code ${code}")

private object ReplFrontendConfig {
  /** Creates a FrontendConfig instance for the REPL
    *
    * This passes through the feature identifiers from the command line and sets the include and package root
    * directories to the current directory
    */
  def apply(targetPlatform: platform.TargetPlatform): frontend.FrontendConfig =  {
    val currentDirUrl = (new File(System.getProperty("user.dir"))).toURI.toURL

    val includePath = frontend.IncludePath(List(currentDirUrl))

    frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=FeatureIdentifiers()
    )
  }
}

/** User datum evaluator
  *
  * This is the core of the REPL implementation. It is responsible for evaluating data and tracking the REPL state.
  */
class Evaluator(targetPlatform: platform.TargetPlatform) {
  private implicit val frontendConfig = ReplFrontendConfig(targetPlatform)

  private val compileConfig = CompileConfig(
    includePath=frontendConfig.includePath,
    optimiseLevel=2,
    targetPlatform=targetPlatform
  )

  val loader: frontend.LibraryLoader = new frontend.LibraryLoader()
  val prefixExprs: mutable.ListBuffer[et.Expr] = new mutable.ListBuffer

  implicit val frontendContext = frontend.FrontendContext(frontendConfig, loader, debug.UnknownContext)

  val initialLibraries = List(
    List("llambda", "base"),
    List("llambda", "write"),
    List("llambda", "internal", "repl"),
    List("llambda", "nfi")
  )

  val initialBindings = initialLibraries.flatMap(loader.load(_))

  var scope: Scope = new Scope(mutable.Map(initialBindings: _*))

  private def exprsToOutputString(exprs: List[et.Expr]): String = {
    val result = Compiler.runExprs(exprs, compileConfig, Nil, None)

    if (result.exitValue != 0) {
      throw new ReplProcessNonZeroExitException(result.exitValue, result.stdout, result.stderr)
    }

    result.stdout
  }

  /** Handles evaluating definitions that only produce abstract bindings
    *
    * This includes (define-type) and (define-syntax)
    */
  private def evalSimpleDefine(defineDatum: ast.Datum): String = {
    // Evaluating this in our top-level scope and collect any expressions required for record type defines
    prefixExprs ++= frontend.ExtractModuleBody(List(defineDatum), scope)

    "defined"
  }

  /** Handles evaluating definitions of values and mutations */
  private def evalValueDefine(defineDatum: ast.Datum, identifier: String): String = {
    // Create a test scope in case binding fails
    val childScope = new Scope(mutable.Map(), Some(scope))

    // Bind the value using the original (define)
    val defineExprs = frontend.ExtractModuleBody(List(defineDatum), childScope)

    // Then print the bound datum
    val printingDatum =
      ast.ProperList(List(
        ast.Symbol("write-stdout"),
        ast.Symbol(identifier)
      ))

    val printingExprs = frontend.ExtractModuleBody(List(printingDatum), childScope)
    val programExprs = loader.libraryExprs ++ prefixExprs.toList ++ defineExprs ++ printingExprs

    val resultString = exprsToOutputString(programExprs)

    val memoizedExprsOpt = try {
      val resultData = SchemeParser.parseStringAsData(resultString)

      resultData match {
        case List(resultDatum) =>
          // We can replace this with a static datum. This means we don't have to re-evaluate it on every REPL program
          //  and unstable values are fixed.
          defineExprs match {
            case List(et.TopLevelDefine(et.Binding(storageLoc, _))) =>
              Some(List(et.TopLevelDefine(et.Binding(storageLoc, et.Literal(resultDatum)))))

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
      case _: ParseErrorException =>
        None
    }

    val newPrefixExprs = memoizedExprsOpt.getOrElse(defineExprs)

    // Save this if the program worked correctly
    prefixExprs ++= newPrefixExprs
    scope = childScope

    // Use a distinct separator for memoized results versus recalculated results
    val separator = if (memoizedExprsOpt.isDefined) "=>" else "->"

    s"${identifier} ${separator} ${resultString}"
  }

  /** Evaluates a datum in the current REPL state
    *
    * @param  userDatum  Datum to evaluate
    * @return User-readable string containing the successful result of the evaluation.
    */
  @throws(classOf[SemanticException])
  @throws(classOf[ReplProcessNonZeroExitException])
  def apply(userDatum: ast.Datum): String = userDatum match {
    case importDecl @ ast.ProperList(ast.Symbol("import") :: _) =>
      // Load the library
      scope.bindings ++= frontend.ResolveImportDecl(importDecl)(loader, frontendConfig)

      "loaded"

    // Simple (define)
    case defineDatum @ ast.ProperList(ast.Symbol(defineIdent) :: ast.Symbol(identifier) :: _)
        if scope.get(defineIdent) == Some(Primitives.Define) =>
      evalValueDefine(defineDatum, identifier)

    // Lambda shorthand
    case defineDatum @ ast.ProperList(ast.Symbol(defineIdent) :: ast.ProperList(ast.Symbol(identifier) :: _) :: _)
        if scope.get(defineIdent) == Some(Primitives.Define) =>
      evalValueDefine(defineDatum, identifier)

    // (set!)
    case defineDatum @ ast.ProperList(ast.Symbol(setIdent) :: ast.Symbol(identifier) :: _)
        if scope.get(setIdent) == Some(Primitives.Set) =>
      evalValueDefine(defineDatum, identifier)

    // (define-syntax)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if scope.get(defineTypeIdent) == Some(Primitives.DefineSyntax) =>
      evalSimpleDefine(defineDatum)

    // (define-type)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if scope.get(defineTypeIdent) == Some(Primitives.DefineType) =>
      evalSimpleDefine(defineDatum)

    // (define-record-type)
    case defineDatum @ ast.ProperList(ast.Symbol(defineTypeIdent) :: _)
        if scope.get(defineTypeIdent) == Some(Primitives.DefineRecordType) =>
      evalSimpleDefine(defineDatum)

    case _ =>
      val printingDatum =
        ast.ProperList(List(
          ast.Symbol("write-stdout"),
          userDatum
        ))

      val printingExprs = frontend.ExtractModuleBody(List(printingDatum), scope)
      val programExprs = loader.libraryExprs ++ prefixExprs.toList ++ printingExprs

      exprsToOutputString(programExprs)
  }
}
