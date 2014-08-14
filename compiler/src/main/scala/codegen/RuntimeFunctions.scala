package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.llvmir.IrFunction._
import llambda.compiler.{celltype => ct}

object RuntimeFunctions {
  val allocCells = IrFunctionDecl(
    result=Result(PointerType(UserDefinedType("cell"))),
    name="_lliby_alloc_cells",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(IntegerType(64))
    ),
    attributes=Set(NoUnwind, Cold)
  )
    
  val signalError = IrFunctionDecl(
    result=IrFunction.Result(VoidType),
    name="_lliby_signal_error",
    arguments=List(
      IrFunction.Argument(PointerType(WorldValue.irType)),
      IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)),
      IrFunction.Argument(PointerType(ct.AnyCell.irType))
    ),
    attributes=Set(NoReturn, Cold)
  )
  
  val dynamicenvPush = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_push",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )

  val dynamicenvSetValue = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_set_value",
    arguments=List(
      Argument(PointerType(WorldValue.irType)),
      Argument(PointerType(ct.ProcedureCell.irType)),
      Argument(PointerType(ct.AnyCell.irType))
    ),
    attributes=Set(NoUnwind)
  )
  
  val dynamicenvPop = IrFunctionDecl(
    result=Result(VoidType),
    name="_lliby_dynamicenv_pop",
    arguments=List(
      Argument(PointerType(WorldValue.irType))
    )
  )
  
  val launchWorld = IrFunctionDecl(
    result=IrFunction.Result(VoidType),
    name="_lliby_launch_world",
    arguments=List(IrFunction.Argument(
      // void (*entryPoint)(World *)
      PointerType(FunctionType(VoidType, List(PointerType(WorldValue.irType))))
    ))
  )

  val recordDataAlloc = IrFunctionDecl(
    result=IrFunction.Result(PointerType(IntegerType(8))),
    name="_lliby_record_data_alloc",
    arguments=List(
      IrFunction.Argument(IntegerType(64))
    ),
    attributes=Set(IrFunction.NoUnwind)
  )
  
  val init = IrFunctionDecl(
    result=IrFunction.Result(VoidType),
    name="lliby_init",
    arguments=Nil,
    attributes=Set(IrFunction.NoUnwind)
  )

  def hasSideEffects(symbol : String, arity : Int) : Boolean = (symbol, arity) match {
    case ("_lliby_stdin_port", 0) =>  false
    case ("_lliby_stdout_port", 0) => false
    case ("_lliby_stderr_port", 0) => false
    case _ => true
  }
}
