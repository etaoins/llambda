package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._
import io.llambda.compiler.NoSourceLocation
import io.llambda.compiler.ImpossibleTypeConversionException

import Implicits._

class ReconcileTypeVarsSuite extends FunSuite {
  val polyNumA = new TypeVar("A", NumberType)
  val polyNumB = new TypeVar("B", NumberType)

  val polyIntC = new TypeVar("C", ExactIntegerType)

  test("reconciling empty vars") {
    val typeVars = Set[TypeVar]()
    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Map())

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with no resolved vars") {
    val typeVars = Set(polyNumA, polyIntC)
    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> NumberType,
      polyIntC -> ExactIntegerType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with partial resolved vars") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> ExactIntegerType
    ))

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> ExactIntegerType,
      polyNumB -> NumberType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with all resolved vars") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> FlonumType,
      polyNumB -> ExactIntegerType
    ))

    val expected = ReconcileTypeVars.Result(Map(
      polyNumA -> FlonumType,
      polyNumB -> ExactIntegerType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with incompatible upper bound fails") {
    val typeVars = Set(polyNumA, polyNumB)
    val resolved = ResolveTypeVars.Result(Map(
      polyNumA -> PortType
    ))

    intercept[ImpossibleTypeConversionException] {
      ReconcileTypeVars(typeVars, NoSourceLocation, resolved)
    }
  }
}
