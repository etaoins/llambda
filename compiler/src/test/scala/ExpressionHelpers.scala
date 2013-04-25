package test.scala

import org.scalatest.FunSuite
import llambda._

trait ExpressionHelpers extends FunSuite {
  def expressionFor(scheme : String)(implicit scope : Scope)  = {
    val (expr :: Nil, _) = bodyFor(scheme)
    expr
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

