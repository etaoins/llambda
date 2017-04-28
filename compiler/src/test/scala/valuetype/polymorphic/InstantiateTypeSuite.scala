package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.valuetype._

import Implicits._


class InstantiateTypeSuite extends FunSuite {
  val polyA = new TypeVar("A")
  val polyB = new TypeVar("B")

  test("instantiating a native type") {
    val polyNative = Int64
    val reconciled = ReconcileTypeVars.Result(Map())
    val expected = Int64

    assert(InstantiateType(reconciled, polyNative) === expected)
  }

  test("instantiating a leaf Scheme type") {
    val polyLeaf = IntegerType
    val reconciled = ReconcileTypeVars.Result(Map())
    val expected = IntegerType

    assert(InstantiateType(reconciled, polyLeaf) === expected)
  }

  test("instantiating a bare type var") {
    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType
    ))

    val expected = IntegerType

    assert(InstantiateType(reconciled, polyA) == expected)
  }

  test("instantiating a pair type") {
    val polyPair = SpecificPairType(polyA, polyB)

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))

    val expected = SpecificPairType(
      IntegerType,
      FlonumType
    )

    assert(InstantiateType(reconciled, polyPair) == expected)
  }

  test("instantiating a nested pair") {
    val polyPair = SpecificPairType(
      polyA,
      SpecificPairType(
        polyA,
        polyA
      )
    )

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType
    ))

    val expected = SpecificPairType(
      IntegerType,
      SpecificPairType(
        IntegerType,
        IntegerType
      )
    )

    assert(InstantiateType(reconciled, polyPair) == expected)
  }

  test("instantiating a union") {
    val polyUnion = UnionType(Set(polyA, BooleanType))

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType
    ))

    val expected = UnionType(Set(
      IntegerType,
      BooleanType
    ))

    assert(InstantiateType(reconciled, polyUnion) == expected)
  }

  test("instantiating a procedure type") {
    val polyProc = ProcedureType(
      mandatoryArgTypes=List(polyA, PortType),
      optionalArgTypes=List(PortType, polyA),
      restArgMemberTypeOpt=Some(polyB),
      returnType=ReturnType.Reachable(polyA)
    )

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))

    val expected = ProcedureType(
      mandatoryArgTypes=List(IntegerType, PortType),
      optionalArgTypes=List(PortType, IntegerType),
      restArgMemberTypeOpt=Some(FlonumType),
      returnType=ReturnType.Reachable(IntegerType)
    )

    assert(InstantiateType(reconciled, polyProc) == expected)
  }

  test("instantiating a case procedure type") {
    val polyCaseProc = CaseProcedureType(List(
      ProcedureType(
        mandatoryArgTypes=List(polyA),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(polyB)
      ),
      ProcedureType(
        mandatoryArgTypes=List(polyA, polyB),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(polyA)
      )
    ))

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))

    val expected = CaseProcedureType(List(
      ProcedureType(
        mandatoryArgTypes=List(IntegerType),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(FlonumType)
      ),
      ProcedureType(
        mandatoryArgTypes=List(IntegerType, FlonumType),
        optionalArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=ReturnType.Reachable(IntegerType)
      )
    ))

    assert(InstantiateType(reconciled, polyCaseProc) == expected)
  }

  test("instantiating a hash map type") {
    val polyHashMap = HashMapType(polyA, polyB)

    val reconciled = ReconcileTypeVars.Result(Map(
      polyA -> IntegerType,
      polyB -> FlonumType
    ))

    val expected = HashMapType(IntegerType, FlonumType)

    assert(InstantiateType(reconciled, polyHashMap) === expected)
  }
}
