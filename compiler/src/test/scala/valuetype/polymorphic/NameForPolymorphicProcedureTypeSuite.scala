package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.valuetype._

class NameForPolymorphicProcedureTypeSuite extends FunSuite {
  val polyA = new TypeVar("A")
  val polyB = new TypeVar("B", NumberType)

  test("(-> <number>)") {
    val polyType = ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=ReturnType.Reachable(NumberType)
    ).toPolymorphic

    assert(NameForPolymorphicProcedureType(polyType) === "(-> <number>)")
  }

  test("(All (A) (-> A <number>))") {
    val polyType = PolymorphicProcedureType(
      typeVars=Set(polyA),
      template=ProcedureType(
        mandatoryArgTypes=List(polyA),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(NumberType)
      )
    )

    assert(NameForPolymorphicProcedureType(polyType) === "(All (A) (-> A <number>))")
  }

  test("(All (A [B : <number>]) (-> A B))") {
    val polyType = PolymorphicProcedureType(
      typeVars=Set(polyA, polyB),
      template=ProcedureType(
        mandatoryArgTypes=List(polyA),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(polyB)
      )
    )

    assert(NameForPolymorphicProcedureType(polyType) === "(All (A [B : <number>]) (-> A B))")
  }

  test("(All (A) (->* (A) ((Listof A)) <number>))") {
    val polyType = PolymorphicProcedureType(
      typeVars=Set(polyA),
      template=ProcedureType(
        mandatoryArgTypes=List(polyA),
        optionalArgTypes=List(UniformProperListType(polyA)),
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(NumberType)
      )
    )

    assert(NameForPolymorphicProcedureType(polyType) === "(All (A) (->* (A) ((Listof A)) <number>))")
  }
}
