package io.llambda.compiler.testutil
import io.llambda

import org.scalatest.{FunSuite,OptionValues}

import llambda.compiler._
import llambda.compiler.frontend.{LibraryLoader, IncludePath}

trait ExprHelpers extends FunSuite with OptionValues {
  // Resolve imports relative to /
  // This corresponds to src/test/scheme in our source
  val resourceBaseUrl = getClass.getClassLoader.getResource("")

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

    val bodyExtractor = new frontend.ModuleBodyExtractor(libraryLoader, frontendConfig)
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
          Some(other)
      }
    )
  }
}

