package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ProcedureSignature, ProcedureAttribute}
import llambda.llvmir._
import llambda.llvmir.IrFunction._

class ProcedureSignatureToIrSuite extends FunSuite {
  test("argless void function") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(VoidType),
        arguments=Nil,
        attributes=Set(NoUnwind)
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("fastcc function taking boolean, unsigned int returning signed int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.Predicate, vt.UInt16),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int32),
      attributes=Set(ProcedureAttribute.FastCC)
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(IntegerType(32), Set(SignExt)),
        arguments=List(Argument(IntegerType(1), Set(ZeroExt)), Argument(IntegerType(16), Set(ZeroExt))),
        attributes=Set(NoUnwind),
        callingConv=CallingConv.FastCC
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("function taking only world and optional args") {
    val procSignature = new ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=List(vt.SymbolType),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(VoidType),
        arguments=List(
          Argument(PointerType(WorldValue.irType)),
          Argument(PointerType(ct.ListElementCell.irType))
        )
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("function taking only world and self args with noreturn attribute") {
    val procSignature = new ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set(ProcedureAttribute.NoReturn)
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(VoidType),
        arguments=List(
          Argument(PointerType(WorldValue.irType)),
          Argument(PointerType(ct.ProcedureCell.irType))
        ),
        attributes=Set(IrFunction.NoReturn)
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("function taking only rest args returning unsigned int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.SymbolType),
      returnType=vt.ReturnType.SingleValue(vt.UInt32),
      attributes=Set()
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(IntegerType(32), Set(ZeroExt)),
        arguments=List(Argument(PointerType(ct.ListElementCell.irType))),
        attributes=Set(NoUnwind)
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("function taking union of port and symbol") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      mandatoryArgTypes=List(vt.UnionType(Set(vt.PortType, vt.SymbolType))),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.AnySchemeType),
      attributes=Set()
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(PointerType(ct.AnyCell.irType)),
        arguments=List(Argument(PointerType(ct.AnyCell.irType))),
        attributes=Set(NoUnwind)
      ),
      callMetadata=Map()
    )

    assert(result === expected)
  }

  test("function taking world, self, two numbers, rest arg returning unicode char") {
    val procSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      mandatoryArgTypes=List(vt.NumberType, vt.NumberType),
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.ExactIntegerType),
      returnType=vt.ReturnType.SingleValue(vt.UnicodeChar),
      attributes=Set()
    )

    val result = ProcedureSignatureToIr(procSignature)

    val expected = ProcedureSignatureToIr.Result(
      irSignature=IrSignature(
        result=Result(IntegerType(32), Set(SignExt)),
        arguments=List(
          Argument(PointerType(WorldValue.irType)),
          Argument(PointerType(ct.ProcedureCell.irType)),
          Argument(PointerType(ct.NumberCell.irType)),
          Argument(PointerType(ct.NumberCell.irType)),
          Argument(PointerType(ct.ListElementCell.irType))
        )
      ),
      callMetadata=Map(
        "range" -> RangeMetadata(IntegerType(32), (0x0, 0x110000))
      )
    )

    assert(result === expected)
  }
}
