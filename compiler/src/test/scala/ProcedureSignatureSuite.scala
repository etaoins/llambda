package io.llambda.compiler
import io.llambda

import io.llambda.compiler.{valuetype => vt}

import org.scalatest.FunSuite

class ProcedureSignatureSuite extends FunSuite {
  test("returning procedure without optional or rest arg") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Double),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set()
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      Nil,
      None,
      vt.ReturnType.Reachable(vt.ExactIntegerType)
    ))
  }

  test("returning procedure with optional, without rest arg") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Double),
      optionalArgTypes=List(vt.ExactIntegerType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set()
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      List(vt.ExactIntegerType),
      None,
      vt.ReturnType.Reachable(vt.ExactIntegerType)
    ))
  }

  test("returning procedure without optional, with rest arg") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Double),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.SymbolType),
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set()
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      Nil,
      Some(vt.SymbolType),
      vt.ReturnType.Reachable(vt.ExactIntegerType)
    ))
  }

  test("non-returning procedure") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Double),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.Int64),
      attributes=Set(ProcedureAttribute.NoReturn)
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      Nil,
      None,
      vt.ReturnType.Reachable(vt.EmptySchemeType)
    ))
  }
}
