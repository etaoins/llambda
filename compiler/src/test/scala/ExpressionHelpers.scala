package test.scala

import org.scalatest.FunSuite
import llambda._

trait ExpressionHelpers extends FunSuite {
  def expressionFor(scheme : String)(implicit scope : Scope)  = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(datum :: Nil, _) =>
        ExtractExpressions(datum)(scope)
      case err =>
        fail(err.toString)
    }
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

