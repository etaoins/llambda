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
    val polyLeaf = IntegerType
    val evidence = IntegerType

    assert(ResolveTypeVars(Set(), polyLeaf, evidence).values == Map())
  }

  test("resolving a type var directly") {
    val polyVar = polyA
    val evidence = IntegerType

    assert(ResolveTypeVars(Set(polyA), polyVar, evidence).values == Map(
      polyA -> IntegerType
    ))
  }

  test("resolving simple pair type with distinct car and cdr variables") {
    val polyPair = SpecificPairType(polyA, polyB)

    val evidence = SpecificPairType(
      IntegerType,
      FlonumType
    )

    assert(ResolveTypeVars(Set(polyA, polyB), polyPair, evidence).values == Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))
  }

  test("resolving a pair with empty scheme type") {
    val polyEmpty = EmptySchemeType
    val evidence = SpecificPairType(IntegerType, FlonumType)

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
      IntegerType,
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

    val evidence = UniformProperListType(IntegerType)

    assert(ResolveTypeVars(Set(polyA), polyList, evidence).values == Map(
      polyA -> IntegerType
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

    val evidence = SpecificProperListType(Vector(IntegerType, FlonumType, IntegerType))

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
      mandatoryArgTypes=List(polyA, polyB),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(polyA),
      returnType=ReturnType.Reachable(polyC)
    )

    val evidence = ProcedureType(
      mandatoryArgTypes=List(IntegerType, FlonumType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(FlonumType),
      returnType=ReturnType.Reachable(PortType)
    )

    val result = ResolveTypeVars(Set(polyA, polyB, polyC), polyProc, evidence)

    assert(result.values == Map(
      polyC -> PortType
    ))
  }

  test("resolving procedure type from procedure type with different but compatible arity") {
    val polyProc = ProcedureType(
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(polyA),
      returnType=ReturnType.Reachable(polyB)
    )

    val evidence = ProcedureType(
      mandatoryArgTypes=List(IntegerType, IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(FlonumType),
      returnType=ReturnType.Reachable(PortType)
    )

    val result = ResolveTypeVars(Set(polyA, polyB), polyProc, evidence)

    assert(result.values == Map(
      polyB -> PortType
    ))
  }

  test("resolve a hash map type") {
    val polyHashMap = HashMapType(polyA, polyB)

    val evidence = HashMapType(IntegerType, FlonumType)

    val result = ResolveTypeVars(Set(polyA, polyB), polyHashMap, evidence)

    assert(result.values === Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))
  }
}
