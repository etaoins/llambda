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
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )
    
    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("trivial procedure signature does not satisfy same procedure with world arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(hasWorldArg=true)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("trivial procedure signature does not satisfy same procedure with self arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(hasSelfArg=true)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("trivial procedure signature does not satisfy same procedure with differing calling conv") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(attributes=Set(ProcedureAttribute.FastCC))

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature taking single Scheme fixed arg satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.NumberType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature taking single native fixed arg satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.Int64),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature taking single native fixed arg does not satisfy the boxed version of the arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.Int64),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      fixedArgTypes=List(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature taking fixed arg does not satisfy procedure without fixed arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.NumberType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(fixedArgTypes=Nil)

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature taking single fixed arg does not satisfy covariant arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.ExactIntegerType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      fixedArgTypes=List(vt.NumberType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature taking single fixed arg satisfies contravariant arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.NumberType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      fixedArgTypes=List(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature taking rest arg does not satisfy covariant rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.ExactIntegerType),
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
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
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.NumberType),
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=Some(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature taking single fixed arg does not satisfy procedure taking rest arg") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.ExactIntegerType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      restArgMemberTypeOpt=Some(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature returning single Scheme value satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.ExactIntegerType),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature returning single Scheme value does not satisfy a single value list") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.ExactIntegerType),
      attributes=Set()
    )

    // We need this gross constructor to avoid SpecificValues returning a SingleValue automatically
    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.MultipleValues(
        vt.SpecificProperListType(List(
          vt.DirectSchemeTypeRef(vt.ExactIntegerType)
        ))
      ))

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature returning single native value satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int64),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature returning single native value does not satisfy boxed version of the value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int64),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SingleValue(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature returning single value satisfies covariant value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.ExactIntegerType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SingleValue(vt.NumberType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature returning single value does not satisfy contravariant value") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.NumberType),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SingleValue(vt.ExactIntegerType)
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }

  
  test("procedure signature returning multiple values satisfies itself") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SpecificValues(List(vt.ExactIntegerType, vt.ExactIntegerType)),
      attributes=Set()
    )

    val superSignature = derivedSignature

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature returning multiple values satisfies covariant values") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SpecificValues(List(vt.ExactIntegerType, vt.ExactIntegerType)),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SpecificValues(List(vt.NumberType, vt.NumberType))
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === true)
  }
  
  test("procedure signature returning multiple values does not satisfy contravariant values") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SpecificValues(List(vt.NumberType, vt.NumberType)),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SpecificValues(List(vt.ExactIntegerType, vt.ExactIntegerType))
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
  
  test("procedure signature returning multiple values does not satisfy differing number of values") {
    val derivedSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SpecificValues(List(vt.NumberType, vt.NumberType)),
      attributes=Set()
    )

    val superSignature = derivedSignature.copy(
      returnType=vt.ReturnType.SpecificValues(List(vt.NumberType, vt.NumberType, vt.NumberType))
    )

    assert(SatisfiesSignature(superSignature, derivedSignature) === false)
  }
}
