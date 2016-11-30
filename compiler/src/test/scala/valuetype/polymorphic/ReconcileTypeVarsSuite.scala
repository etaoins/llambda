package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.valuetype._
import llambda.compiler.NoSourceLocation
import llambda.compiler.TypeException

import Implicits._

class ReconcileTypeVarsSuite extends FunSuite {
  val polyNumA = new TypeVar("A", NumberType)
  val polyNumB = new TypeVar("B", NumberType)

  val polyIntC = new TypeVar("C", IntegerType)

  test("reconciling empty vars") {
    val typeVars = Set[TypeVar]()
    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Map())

    assert(ReconcileTypeVars(typeVars, resolved) === expected)
  }

  test("reconciling vars with no resolved vars") {
    val typeVars = Set(polyNumA, polyIntC)
    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> NumberType,
      polyIntC -> IntegerType
    ))

    assert(ReconcileTypeVars(typeVars, resolved) === expected)
  }

  test("reconciling vars with partial resolved vars") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> IntegerType
    ))

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> IntegerType,
      polyNumB -> NumberType
    ))

    assert(ReconcileTypeVars(typeVars, resolved) === expected)
  }

  test("reconciling vars with all resolved vars") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> FlonumType,
      polyNumB -> IntegerType
    ))

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> FlonumType,
      polyNumB -> IntegerType
    ))

    assert(ReconcileTypeVars(typeVars, resolved) === expected)
  }

  test("reconciling vars with incompatible upper bound defaults to upper bound") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> PortType
    ))

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> NumberType,
      polyNumB -> NumberType
    ))

    assert(ReconcileTypeVars(typeVars, resolved) === expected)
  }

  test("reconciling vars with incompatible upper bound fails in strict mode") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> PortType
    ))

    intercept[TypeException] {
      ReconcileTypeVars(typeVars, resolved, NoSourceLocation, true)
    }
  }

}
