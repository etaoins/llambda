package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import org.scalatest.FunSuite

import io.llambda.compiler.valuetype._
import io.llambda.compiler.NoSourceLocation
import io.llambda.compiler.ImpossibleTypeConversionException

import Implicits._

class ReconcileTypeVarsSuite extends FunSuite {
  test("reconciling empty vars") {
    val typeVars = Vector()
    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Vector())

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with no resolved vars") {
    val typeVars = Vector(
      TypeVar(ExactIntegerType),
      TypeVar(FlonumType)
    )

    val resolved = ResolveTypeVars.Result()

    val expected = ReconcileTypeVars.Result(Vector(
      ExactIntegerType,
      FlonumType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with partial resolved vars") {
    val typeVars = Vector(
      TypeVar(NumberType),
      TypeVar(NumberType)
    )

    val resolved = ResolveTypeVars.Result(Map(
      1 -> ExactIntegerType
    ))

    val expected = ReconcileTypeVars.Result(Vector(
      NumberType,
      ExactIntegerType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with all resolved vars") {
    val typeVars = Vector(
      TypeVar(NumberType),
      TypeVar(NumberType)
    )

    val resolved = ResolveTypeVars.Result(Map(
      0 -> FlonumType,
      1 -> ExactIntegerType
    ))

    val expected = ReconcileTypeVars.Result(Vector(
      FlonumType,
      ExactIntegerType
    ))

    assert(ReconcileTypeVars(typeVars, NoSourceLocation, resolved) === expected)
  }

  test("reconciling vars with incompatible upper bound fails") {
    val typeVars = Vector(
      TypeVar(NumberType),
      TypeVar(NumberType)
    )

    val resolved = ResolveTypeVars.Result(Map(
      0 -> PortType
    ))

    intercept[ImpossibleTypeConversionException] {
      ReconcileTypeVars(typeVars, NoSourceLocation, resolved)
    }
  }
}
