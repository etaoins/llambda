package llambda.testutil

import org.scalatest.{FunSuite,OptionValues}

import llambda._
import llambda.frontend.{LibraryLoader, IncludePath}

trait ExpressionHelpers extends FunSuite with OptionValues {
  // Resolve imports relative to /
  // This corresponds to src/test/scheme in our source
  val resourceBaseUrl = getClass.getClassLoader.getResource("")

  val libraryLoader = new LibraryLoader
  
  val includePath = frontend.IncludePath(
    fileParentDir=Some(resourceBaseUrl),
    packageRootDir=Some(resourceBaseUrl)
  )

  def expressionFor(scheme : String)(implicit scope : Scope) = {
    val (expr :: Nil) = bodyFor(scheme)(scope)
    expr
  }
  
  def bindingFor(scheme : String, varName : String)(scope : Scope)  = {
    bodyFor(scheme)(scope)
    scope.get(varName).value
  }
  
  def bodyFor(scheme : String)(scope : Scope) = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(data, _) =>
        frontend.ExtractModuleBody(data)(scope, libraryLoader, includePath)
      case err =>
        fail(err.toString)
    }
  }
}

