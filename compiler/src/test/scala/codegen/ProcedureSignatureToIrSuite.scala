package io.llambda.compiler.codegen
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ProcedureSignature
import llambda.llvmir._
import llambda.llvmir.IrFunction._

class ProcedureSignatureToIrSuite extends FunSuite {
  test("argless void function") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgs=Nil,
      hasRestArg=false,
      returnType=None
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=Nil
    ))
  }
  
  test("function taking boolean, unsigned int returning signed int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgs=List(vt.CBool, vt.UInt16),
      hasRestArg=false,
      returnType=Some(vt.Int32)
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(SignExt)),
      arguments=Argument(IntegerType(8), Set(ZeroExt)) :: Argument(IntegerType(16), Set(ZeroExt)) :: Nil
    ))
  }
  
  test("function taking only world and self args returning void") {
    val procSignature = new ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgs=Nil,
      hasRestArg=false,
      returnType=None
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(VoidType),
      arguments=List(
        Argument(PointerType(WorldValue.irType)),
        Argument(PointerType(ct.ProcedureCell.irType))
      )
    ))
  }
  
  test("function taking only rest args returning unsigned int") {
    val procSignature = ProcedureSignature(
      hasWorldArg=false,
      hasSelfArg=false,
      fixedArgs=Nil,
      hasRestArg=true,
      returnType=Some(vt.UInt32)
    )

    val irSignature = ProcedureSignatureToIr(procSignature)

    assert(irSignature === IrSignature(
      result=Result(IntegerType(32), Set(ZeroExt)),
      arguments=Argument(PointerType(ct.ListElementCell.irType)) :: Nil
    ))
  }
  
  test("function taking world, self, two numerics, rest arg returning rational") {
    val procSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=true,
      fixedArgs=List(vt.IntrinsicCellType(ct.NumericCell), vt.IntrinsicCellType(ct.NumericCell)),
      hasRestArg=true,
      returnType=Some(vt.IntrinsicCellType(ct.InexactRationalCell))
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
