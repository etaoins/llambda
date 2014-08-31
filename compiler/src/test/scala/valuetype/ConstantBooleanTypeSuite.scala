package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import Implicits._

class ConstantBooleanTypeSuite extends SchemeTypeSuite {
  test("the union of both constant booleans is the general boolean") {
    assert(SchemeType.fromTypeUnion(List(constantFalse, constantTrue)) === BooleanType)
  }

  test("general boolean type minus a constant boolean is the other constant boolean") {
    assert((BooleanType - constantTrue) === constantFalse)
    assert((BooleanType - constantFalse) === constantTrue)
  }

  test("intersection of the constant booleans in an empty union") {
    assertIntersection(constantTrue, constantFalse, EmptySchemeType)
  }

  test("intersection of a constant boolean with the general boolean is itself") {
    assertIntersection(constantTrue, BooleanType, constantTrue)
    assertIntersection(constantFalse, BooleanType, constantFalse)
  }
  
  test("boolean constants satisfy themselvs") {
    assert(SatisfiesType(constantFalse, constantFalse) === Some(true))
    assert(SatisfiesType(constantTrue, constantTrue) === Some(true))
  }

  test("boolean constants satisfy the general boolean type") {
    assert(SatisfiesType(BooleanType, constantFalse) === Some(true))
    assert(SatisfiesType(BooleanType, constantTrue) === Some(true))
  }
}
