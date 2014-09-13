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
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=Nil,
      attributes=Set(NoUnwind)
    ))
  }
  
  test("fastcc function taking boolean, unsigned int returning signed int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.Predicate, vt.UInt16),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.Int32),
      attributes=Set(ProcedureAttribute.FastCC)
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(SignExt)),
      arguments=List(Argument(IntegerType(1), Set(ZeroExt)), Argument(IntegerType(16), Set(ZeroExt))),
      attributes=Set(NoUnwind),
      callingConv=CallingConv.FastCC
    ))
  }
  
  test("function taking only world and self args with noreturn attribute") {
    val procSignature = new ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set(ProcedureAttribute.NoReturn)
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType))
      ),
      attributes=Set(IrFunction.NoReturn)
    ))
  }
  
  test("function taking only rest args returning unsigned int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=Some(vt.SymbolType),
      returnType=vt.ReturnType.SingleValue(vt.UInt32),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=List(Argument(PointerType(ct.ListElementCell.irType))),
      attributes=Set(NoUnwind)
    ))
  }
  
  test("function taking union of port and symbol returning arbitrary multiple values") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgTypes=List(vt.UnionType(Set(vt.PortType, vt.SymbolType))),
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.ArbitraryValues,
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.ListElementCell.irType)),
      arguments=List(Argument(PointerType(ct.AnyCell.irType))),
      attributes=Set(NoUnwind)
    ))
  }
  
  test("function taking world, self, two numbers, rest arg returning rational") {
    val procSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgTypes=List(vt.NumberType, vt.NumberType),
      restArgMemberTypeOpt=Some(vt.ExactIntegerType),
      returnType=vt.ReturnType.SingleValue(vt.FlonumType),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.FlonumCell.irType)),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType)),
        Argument(PointerType(ct.NumberCell.irType)),
        Argument(PointerType(ct.NumberCell.irType)), 
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }
  
  test("function only self and returning specific multiple values") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=true,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SpecificValues(List(vt.FlonumType, vt.ExactIntegerType)),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.PairCell.irType)),
      arguments=List(
        Argument(PointerType(ct.ProcedureCell.irType))
      ),
      attributes=Set(NoUnwind)
    ))
  }

  test("adapted procedure signature") {
    val irSignature = ProcedureSignatureToIr(AdaptedProcedureSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.ListElementCell.irType)),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType)),
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }
}
