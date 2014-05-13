package io.llambda.compiler.testutil
import io.llambda

import org.scalatest.{FunSuite,OptionValues}

import llambda.compiler._
import llambda.compiler.frontend.{LibraryLoader, IncludePath}

trait ExpressionHelpers extends FunSuite with OptionValues {
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

  def expressionFor(scheme : String)(implicit scope : Scope) = {
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

    // Analyize libraries + user exprs
    val allExprs = libraryLoader.libraryExpressions ++ userExprs
    val analysis = analyzer.Analyize(allExprs)

    reducer.ReduceExpressions(userExprs)(analysis)
  }

  def bindlessReductionFor(schemeString : String)(implicit scope : Scope) = {
    // This removes any top-level Bind()s for lambdas
    // Just one pass of the reducer will leave the un-inlined versions of procedures behind. This is because it doesn't
    // know they're unused until all the callers are inlined. One solution would be to run the reducer twice but we 
    // want to ensure that the full inlining operation can happen in one pass.
    et.Expression.fromSequence(
      reductionFor(schemeString).toSequence.flatMap {
        case et.TopLevelDefinition(_) =>
          None

        case other =>
          Some(other)
      }
    )
  }

}

