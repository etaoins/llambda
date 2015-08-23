package io.llambda.compiler
import io.llambda

import io.llambda.compiler.{valuetype => vt}

import org.scalatest.FunSuite

class ProcedureSignatureSuite extends FunSuite {
  test("returning procedure") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      fixedArgTypes=List(vt.Double),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int64),
      attributes=Set()
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      Nil,
      None,
      vt.ReturnType.SingleValue(vt.ExactIntegerType)
    ))
  }

  test("non-returning procedure") {
    val returningSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      fixedArgTypes=List(vt.Double),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int64),
      attributes=Set(ProcedureAttribute.NoReturn)
    )

    assert(returningSignature.toSchemeProcedureType === vt.ProcedureType(
      List(vt.FlonumType),
      Nil,
      None,
      vt.ReturnType.SingleValue(vt.EmptySchemeType)
    ))
  }
}
