package llambda.testutil

import org.scalatest.{FunSuite,OptionValues}
import llambda._

trait ExpressionHelpers extends FunSuite with OptionValues {
  def expressionFor(scheme : String)(implicit scope : Scope)  = {
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
        frontend.ExtractBody(data)(scope)
      case err =>
        fail(err.toString)
    }
  }
}

