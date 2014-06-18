package io.llambda.compiler.testutil
import io.llambda

import org.scalatest.{FunSuite,OptionValues}

import llambda.compiler._
import llambda.compiler.frontend.{LibraryLoader, IncludePath}

trait ExprHelpers extends FunSuite with OptionValues {
  // Resolve imports relative to /
  // This corresponds to src/test/scheme in our source
  val resourceBaseUrl = getClass.getClassLoader.getResource("")

  val debugContext = debug.UnknownContext
  val libraryLoader = new LibraryLoader(platform.Posix64LE)
  
  val includePath = frontend.IncludePath(
    fileParentDir=Some(resourceBaseUrl),
    packageRootDir=Some(resourceBaseUrl)
  )

  val frontendConfig = frontend.FrontendConfig(
    includePath=includePath,
    featureIdentifiers=FeatureIdentifiers(platform.Posix64LE)
  )
  
  val schemeBaseBindings = libraryLoader.loadSchemeBase(frontendConfig)
  def schemeBaseScope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))
  
  val typedLambdaBindings = libraryLoader.load(List("llambda", "typed").map(StringComponent(_)), NoSourceLocation)(frontendConfig)
  def typedLambdaScope = new Scope(collection.mutable.Map((schemeBaseBindings ++ typedLambdaBindings).toSeq : _*))

  def exprFor(scheme : String)(implicit scope : Scope) = {
    val (expr :: Nil) = bodyFor(scheme)(scope)
    expr
  }
  
  def bindingFor(scheme : String, varName : String)(scope : Scope)  = {
    bodyFor(scheme)(scope)
    scope.get(varName).value
  }
  
  def bodyFor(scheme : String)(scope : Scope) = {
    val data = SchemeParser.parseStringAsData(scheme)

    val bodyExtractor = new frontend.ModuleBodyExtractor(debugContext, libraryLoader, frontendConfig)
    bodyExtractor(data, scope)
  }

  def reductionFor(scheme : String)(implicit scope : Scope) = {
    val userExprs = bodyFor(scheme)(scope)

    // Analyse libraries + user exprs
    val allExprs = libraryLoader.libraryExprs ++ userExprs
    val analysis = reducer.AnalyseExprs(allExprs)

    et.Expr.fromSequence(
      // Remove all top level defines
      reducer.ReduceExprs(analysis).toSequence.flatMap {
        case et.TopLevelDefinition(_) =>
          None

        case other =>
          // Make sure this is located
          assertExprLocated(other)
          Some(other)
      }
    )
  }

  def assertExprLocated(expr : et.Expr) {
    expr match {
      case et.TopLevelDefinition(List((reportProc : ReportProcedure, _))) if reportProc.reportName == "features" =>
        // This is an artificial procedure - don't check subexpressions
        return

      case _ : et.Begin =>
      case _ : et.InternalDefinition =>
      case _ : et.TopLevelDefinition =>
        // These are structural - can be unlocated

      case other =>
        assert(other.hasLocation, s"Expression is unlocated: ${other.toString}")
    }

    expr.subexprs.map(assertExprLocated)
  }
}

