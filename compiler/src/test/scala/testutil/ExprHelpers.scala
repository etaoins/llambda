package io.llambda.compiler.testutil
import io.llambda

import org.scalatest.{FunSuite,OptionValues}

import llambda.compiler._
import llambda.compiler.frontend.LibraryLoader


trait ExprHelpers extends FunSuite with OptionValues {
  // Resolve imports relative to /
  // This corresponds to src/test/scheme in our source
  val resourceBaseUrl = getClass.getClassLoader.getResource("")

  val debugContext = debug.UnknownContext
  val libraryLoader = new LibraryLoader()

  val includePath = frontend.IncludePath(List(resourceBaseUrl))

  val frontendConfig = frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=FeatureIdentifiers()
    )

  val schemeBaseBindings = libraryLoader.loadSchemeBase(frontendConfig)
  def schemeBaseScope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq: _*))

  val typedLambdaBindings = libraryLoader.load(List("llambda", "typed"), NoSourceLocation)(frontendConfig)
  def typedLambdaScope = new Scope(collection.mutable.Map((schemeBaseBindings ++ typedLambdaBindings).toSeq: _*))

  def exprFor(scheme: String)(implicit scope: Scope) = {
    val (expr :: Nil) = bodyFor(scheme)(scope)
    expr
  }

  def bindingFor(scheme: String, varName: String)(scope: Scope)  = {
    bodyFor(scheme)(scope)
    scope.get(varName).value
  }

  def bodyFor(scheme: String)(scope: Scope) = {
    val data = SchemeParser.parseStringAsData(scheme)

    val frontendContext = frontend.FrontendContext(
      frontendConfig,
      libraryLoader,
      debugContext
    )

    val exprs = frontend.ExtractModuleBody(data, scope)(frontendContext)

    frontend.FinishScope(scope)

    exprs
  }

  def assertExprLocated(expr: et.Expr) {
    expr match {
      case et.TopLevelDefine(et.Binding(stdlibProc: StdlibProcedure, _))
          if stdlibProc.stdlibName == "features" =>
        // This is an artificial procedure - don't check subexpressions
        return

      case _: et.Begin =>
      case _: et.InternalDefine =>
      case _: et.TopLevelDefine =>
        // These are structural - can be unlocated

      case other =>
        assert(other.hasLocation, s"Expression is unlocated: ${other.toString}")
    }

    expr.subexprs.map(assertExprLocated)
  }
}

