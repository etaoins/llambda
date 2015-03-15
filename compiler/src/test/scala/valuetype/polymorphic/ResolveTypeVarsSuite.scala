package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.valuetype._
import Implicits._

class ResolveTypeVarsSuite extends FunSuite {
  val polyA = new TypeVar("A")
  val polyB = new TypeVar("B")
  val polyC = new TypeVar("C")

  test("resolving leaf types") {
    val polyLeaf = ExactIntegerType
    val evidence = ExactIntegerType

    assert(ResolveTypeVars(Set(), polyLeaf, evidence).values == Map())
  }

  test("resolving a type var directly") {
    val polyVar = polyA
    val evidence = ExactIntegerType

    assert(ResolveTypeVars(Set(polyA), polyVar, evidence).values == Map(
      polyA -> ExactIntegerType
    ))
  }

  test("resolving simple pair type with distinct car and cdr variables") {
    val polyPair = SpecificPairType(polyA, polyB)

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(Set(polyA, polyB), polyPair, evidence).values == Map(
      polyA -> ExactIntegerType,
      polyB -> FlonumType
    ))
  }

  test("resolving a pair with empty scheme type") {
    val polyEmpty = EmptySchemeType
    val evidence = SpecificPairType(ExactIntegerType, FlonumType)

    assert(ResolveTypeVars(Set(polyA), polyEmpty, evidence).values == Map())
  }

  test("resolving the empty scheme type with pair") {
    val polyPair = SpecificPairType(polyA, polyB)
    val evidence = EmptySchemeType

    assert(ResolveTypeVars(Set(polyA), polyPair, evidence).values == Map())
  }

  test("resolving simple pair type with same car and cdr variables") {
    val polyPair = SpecificPairType(polyA, polyA)

    val evidence = SpecificPairType(
      ExactIntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(Set(polyA), polyPair, evidence).values == Map(
      polyA -> NumberType
    ))
  }

  test("resolving uniform proper list type with uniform proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        polyA,
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = UniformProperListType(ExactIntegerType)

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> ExactIntegerType
    ))
  }

  test("resolving uniform proper list type with specific proper list") {
    val polyList = UnionType(Set(
      EmptyListType,
      SpecificPairType(
        polyA,
        RecursiveSchemeTypeRef(1)  // Inner recursive var
      )
    ))

    val evidence = SpecificProperListType(Vector(ExactIntegerType, FlonumType, ExactIntegerType))

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> NumberType
    ))
  }

  test("resolving specific proper list type with uniform proper list") {
    val polyList =
      SpecificPairType(
        polyA,
        SpecificPairType(
          polyA,
          SpecificPairType(
            polyA,
            EmptyListType
          )
        )
      )

    val evidence = UniformProperListType(PortType)

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> PortType
    ))
  }

  test("resolving procedure type from procedure type with identical arity") {
    val polyProc = ProcedureType(
      fixedArgTypes=List(polyA, polyB),
      restArgMemberTypeOpt=Some(polyA),
      returnType=ReturnType.SingleValue(polyC)
    )

    val evidence = ProcedureType(
      fixedArgTypes=List(ExactIntegerType, FlonumType),
      restArgMemberTypeOpt=Some(FlonumType),
      returnType=ReturnType.SingleValue(PortType)
    )

    val result = ResolveTypeVars(Set(polyA, polyB, polyC), polyProc, evidence)

    assert(result.values == Map(
      polyC -> PortType
    ))
  }

  test("resolving procedure type from procedure type with different but compatible arity") {
    val polyProc = ProcedureType(
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=Some(polyA),
      returnType=ReturnType.SpecificValues(List(polyB, polyC))
    )

    val evidence = ProcedureType(
      fixedArgTypes=List(ExactIntegerType, ExactIntegerType),
      restArgMemberTypeOpt=Some(FlonumType),
      returnType=ReturnType.SpecificValues(List(PortType, BooleanType))
    )

    val result = ResolveTypeVars(Set(polyA, polyB, polyC), polyProc, evidence)

    assert(result.values == Map(
      polyB -> PortType,
      polyC -> BooleanType
    ))
  }
}
