package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._

class NameForPolymorphicProcedureTypeSuite extends FunSuite {
  val polyA = new TypeVar("A")
  val polyB = new TypeVar("B", NumberType)

  test("(-> <number>)") {
    val polyType = ProcedureType(
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.SingleValue(NumberType)
    ).toPolymorphic

    assert(NameForPolymorphicProcedureType(polyType) === "(-> <number>)")
  }

  test("(All (A) (-> A <number>))") {
    val polyType = PolymorphicProcedureType(
      typeVars=Set(polyA),
      template=ProcedureType(
        fixedArgTypes=List(polyA),
        restArgMemberTypeOpt=None,
        returnType=ReturnType.SingleValue(NumberType)
      )
    )

    assert(NameForPolymorphicProcedureType(polyType) === "(All (A) (-> A <number>))")
  }

  test("(All (A [B : <number>]) (-> A B))") {
    val polyType = PolymorphicProcedureType(
      typeVars=Set(polyA, polyB),
      template=ProcedureType(
        fixedArgTypes=List(polyA),
        restArgMemberTypeOpt=None,
        returnType=ReturnType.SingleValue(polyB)
      )
    )

    assert(NameForPolymorphicProcedureType(polyType) === "(All (A [B : <number>]) (-> A B))")
  }
}
