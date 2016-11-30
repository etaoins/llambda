package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import llambda.compiler.{valuetype => vt}
import org.scalatest.FunSuite

class SatisfiesSignatureSuite extends FunSuite {
  test("trivial procedure signature taking nothing satifies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("trivial procedure signature does not satisfy same procedure with world arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(hasWorldArg=true)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("trivial procedure signature does not satisfy same procedure with self arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(hasSelfArg=true)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("trivial procedure signature does not satisfy same procedure with differing calling conv") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(attributes=Set(ProcedureAttribute.FastCC))

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature taking single Scheme fixed arg satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.NumberType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature taking single native fixed arg satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Int64),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature taking single native fixed arg does not satisfy the boxed version of the arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Int64),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      mandatoryArgTypes=List(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature taking fixed arg does not satisfy procedure without fixed arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.NumberType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(mandatoryArgTypes=Nil)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature taking single fixed arg does not satisfy covariant arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      mandatoryArgTypes=List(vt.NumberType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature taking single fixed arg satisfies contravariant arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.NumberType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      mandatoryArgTypes=List(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature taking rest arg does not satisfy covariant rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.IntegerType),
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=Some(vt.NumberType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature taking rest arg does satisfy contravariant rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.NumberType),
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=Some(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature taking single fixed arg does not satisfy procedure taking rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.IntegerType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=Some(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature returning single Scheme value satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.IntegerType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature returning single native value satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature returning single native value does not satisfy boxed version of the value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.Reachable(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure signature returning value satisfies covariant value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.IntegerType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.Reachable(vt.NumberType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }

  test("procedure signature returning value does not satisfy contravariant value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.NumberType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.Reachable(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure returning void does not satisfy procedure returning value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.NumberType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.Reachable(vt.AnySchemeType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure with optional arg does not satisfy procedure with same typed rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=List(vt.IntegerType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.IntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure with rest arg does not satisfy procedure without args") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.IntegerType),
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=None
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  test("procedure with rest arg satisfies procedure with optional arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.IntegerType),
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      optionalArgTypes=List(vt.IntegerType),
      restArgMemberTypeOpt=None
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
}
