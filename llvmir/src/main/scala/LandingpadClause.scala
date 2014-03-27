package io.llambda.llvmir

sealed abstract class LandingpadClause extends Irable

case class CatchClause(exceptionClass : GlobalVariable) extends LandingpadClause {
  def toIr = s"catch ${exceptionClass.toIrWithType}"
}

case class FilterClause(exceptionClassType : FirstClassType, exceptionClasses : Seq[GlobalVariable]) extends LandingpadClause {
  private def filterArray = 
    ArrayConstant(exceptionClassType, exceptionClasses)

  def toIr = s"filter ${filterArray.toIrWithType}"
}
