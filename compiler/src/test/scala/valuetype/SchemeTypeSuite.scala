package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

trait SchemeTypeSuite extends FunSuite {
  protected val recordAtomType = SchemeTypeAtom(ct.RecordCell)

  protected val recordType1 = new RecordType("record1", None, Nil)
  protected val recordType1Child1 = new RecordType("record1Child1", Some(recordType1), Nil)
  protected val recordType1Child2 = new RecordType("record1Child2", Some(recordType1), Nil)

  protected val recordType2 = new RecordType("record2", None, Nil)

  protected val literalTrue = LiteralBooleanType(true)
  protected val literalFalse = LiteralBooleanType(false)

  protected val stringList = UniformProperListType(StringType)
  protected val numericList = UniformProperListType(NumberType)
  protected val exactIntList = UniformProperListType(ExactIntegerType)
  protected val inexactList = UniformProperListType(FlonumType)
    
  protected val knownNumberList = PairType(NumberType,
    PairType(ExactIntegerType,
      PairType(FlonumType,
        EmptyListType)))
  
  protected def nonEmptyProperList(memberType : SchemeType) : SchemeType = 
    PairType(memberType, UniformProperListType(memberType))

  protected def assertIntersection(type1 : SchemeType, type2 : SchemeType, resultType : SchemeType) {
    assert((type1 & type2) === resultType) 
    assert((type2 & type1) === resultType) 
  }
}
