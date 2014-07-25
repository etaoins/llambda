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
      fixedArgs=Nil,
      restArgOpt=None,
      returnType=None,
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=Nil,
      attributes=Set(NoUnwind)
    ))
  }
  
  test("function taking boolean, unsigned int returning signed int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgs=List(vt.Predicate, vt.UInt16),
      restArgOpt=None,
      returnType=Some(vt.Int32),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(SignExt)),
      arguments=List(Argument(IntegerType(1), Set(ZeroExt)), Argument(IntegerType(16), Set(ZeroExt))),
      attributes=Set(NoUnwind)
    ))
  }
  
  test("function taking only world and self args with noreturn attribute") {
    val procSignature = new ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgs=Nil,
      restArgOpt=None,
      returnType=None,
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
      fixedArgs=Nil,
      restArgOpt=Some(vt.SymbolType),
      returnType=Some(vt.UInt32),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=List(Argument(PointerType(ct.ListElementCell.irType))),
      attributes=Set(NoUnwind)
    ))
  }
  
  test("function taking world, self, two numerics, rest arg returning rational") {
    val procSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgs=List(vt.NumericType, vt.NumericType),
      restArgOpt=Some(vt.ExactIntegerType),
      returnType=Some(vt.InexactRationalType),
      attributes=Set()
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.InexactRationalCell.irType)),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType)),
        Argument(PointerType(ct.NumericCell.irType)),
        Argument(PointerType(ct.NumericCell.irType)), 
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }

  test("adapted procedure signature") {
    val irSignature = ProcedureSignatureToIr(AdaptedProcedureSignature)

    assert(irSignature === IrSignature(
      result=Result(PointerType(ct.DatumCell.irType)),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType)),
        Argument(PointerType(ct.ListElementCell.irType))
      )
    ))
  }
}
