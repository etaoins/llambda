package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.InternalCompilerErrorException

import org.scalatest.FunSuite

class BoxedTypeSuite extends FunSuite {
  test("create constant boxed inexact rational") {
    val innerValue = llvmir.DoubleConstant(31.5)

    val constant = bt.BoxedInexactRational.createConstant(innerValue)
    val expectedTypeId = bt.BoxedInexactRational.typeId.toString

    assert(constant.toIr === 
      s"{%numeric {%datum {i16 ${expectedTypeId}, i16 0}}, double 31.5}"
    )
  }
  
  test("create constant with incorrect type") {
  }
}
