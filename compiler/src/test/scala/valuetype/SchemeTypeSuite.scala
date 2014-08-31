package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

trait SchemeTypeSuite extends FunSuite {
  protected val recordAtomType = SchemeTypeAtom(ct.RecordCell)

  protected val recordType1 = new RecordType("record1", Nil)
  protected val recordType2 = new RecordType("record1", Nil)

  protected val constantTrue = ConstantBooleanType(true)
  protected val constantFalse = ConstantBooleanType(false)

  protected val stringList = ProperListType(StringType)
  protected val numericList = ProperListType(NumberType)
  protected val exactIntList = ProperListType(ExactIntegerType)
  protected val inexactList = ProperListType(FlonumType)
    
  protected val knownNumberList = PairType(NumberType,
    PairType(ExactIntegerType,
      PairType(FlonumType,
        EmptyListType)))
  
  protected def nonEmptyProperList(memberType : SchemeType) : SchemeType = 
    PairType(memberType, ProperListType(memberType))

  protected def assertIntersection(type1 : SchemeType, type2 : SchemeType, resultType : SchemeType) {
    assert((type1 & type2) === resultType) 
    assert((type2 & type1) === resultType) 
  }
}
