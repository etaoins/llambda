package test.scala.util

import org.scalatest.{FunSuite,OptionValues}
import llambda._

trait ExpressionHelpers extends FunSuite with OptionValues {
  def expressionFor(scheme : String)(implicit scope : Scope)  = {
    val (expr :: Nil, _) = bodyFor(scheme)
    expr
  }
  
  def bindingFor(scheme : String, varName : String)(implicit scope : Scope)  = {
    val (_, newScope) = bodyFor(scheme)
    newScope.get(varName).value
  }
  
  def bodyFor(scheme : String)(implicit scope : Scope) = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(data, _) =>
        ExtractBody(data)(scope)
      case err =>
        fail(err.toString)
    }
  }
}

