package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.llvmir.IrFunction._
import llambda.compiler.ProcedureSignature
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

object RuntimeFunctions {
  val allocCells = IrFunctionDecl(
    result=Result(PointerType(UserDefinedType("cell"))),
    name="llcore_alloc_cells",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(IntegerType(64))
    ),
    attributes=Set(NoUnwind, Cold)
  )

  val signalError = IrFunctionDecl(
    result=IrFunction.Result(VoidType),
    name="llcore_signal_error",
    arguments=List(
      IrFunction.Argument(PointerType(WorldValue.irType)),
      IrFunction.Argument(IntegerType(16)),
      IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(ct.AnyCell.irType)),
      IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)),
      IrFunction.Argument(IntegerType(32))
    ),
    attributes=Set(NoReturn, Cold)
  )

  val dynamicenvPush = IrFunctionDecl(
    result=Result(VoidType),
    name="llcore_dynamicenv_push",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )

  val dynamicenvSetValue = IrFunctionDecl(
    result=Result(VoidType),
    name="llcore_dynamicenv_set_value",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(PointerType(ct.ProcedureCell.irType)),
      Argument(PointerType(ct.AnyCell.irType))
    )
  )

  val dynamicenvPop = IrFunctionDecl(
    result=Result(VoidType),
    name="llcore_dynamicenv_pop",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )

  val run = IrFunctionDecl(
    result=IrFunction.Result(VoidType),
    name="llcore_run",
    arguments=List(
      IrFunction.Argument(
        // void (*entryPoint)(World *)
        PointerType(FunctionType(VoidType, List(PointerType(WorldValue.irType))))
      ),
      IrFunction.Argument(IntegerType(32)),
      IrFunction.Argument(PointerType(PointerType(IntegerType(8)))),
      IrFunction.Argument(IntegerType(1))
    ),
    attributes=Set(IrFunction.NoUnwind)
  )

  val recordDataAlloc = IrFunctionDecl(
    result=IrFunction.Result(PointerType(IntegerType(8))),
    name="llcore_record_data_alloc",
    arguments=List(
      IrFunction.Argument(IntegerType(64))
    ),
    attributes=Set(IrFunction.NoUnwind)
  )

  val vectorAlloc = IrFunctionDecl(
    result=IrFunction.Result(PointerType(ct.VectorCell.irType)),
    name="llcore_vector_alloc",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      IrFunction.Argument(IntegerType(64))
    )
  )

  val makeParameter = IrFunctionDecl(
    result=Result(PointerType(ct.ProcedureCell.irType)),
    name="llcore_make_parameter",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(PointerType(ct.AnyCell.irType))
    )
  )

  val isEqvSymbol = "llcore_is_eqv"
  val isEqualSymbol = "llcore_is_equal"

  val equivalenceProcSignature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    mandatoryArgTypes=List(vt.AnySchemeType, vt.AnySchemeType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.Predicate),
    attributes=Set()
  )

  val symbolIsEqvSymbol = "llcore_symbol_is_eqv"

  val symbolIsEqvSignature = ProcedureSignature(
    hasWorldArg=false,
    hasSelfArg=false,
    mandatoryArgTypes=List(vt.SymbolType, vt.SymbolType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.Predicate),
    attributes=Set()
  )

  val valueForParameterSignature = ProcedureSignature(
    hasWorldArg=true,
    hasSelfArg=true,
    mandatoryArgTypes=Nil,
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.AnySchemeType),
    attributes=Set()
  )

  val valueForParameter = IrFunctionDecl(
    result=Result(PointerType(ct.AnyCell.irType)),
    name="llcore_value_for_parameter",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(PointerType(ct.ProcedureCell.irType))
    ),
    attributes=Set(IrFunction.NoUnwind, IrFunction.ReadOnly)
  )

  def hasSideEffects(symbol: String, arity: Int): Boolean = (symbol, arity) match {
    case ("llcore_stdin_port", 0) =>  false
    case ("llcore_stdout_port", 0) => false
    case ("llcore_stderr_port", 0) => false
    case _ => true
  }
}
