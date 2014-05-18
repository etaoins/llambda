package io.llambda.compiler.codegen
import io.llambda

import scala.io.Source

import llambda.compiler._
import llambda.llvmir._
import llambda.compiler.platform.TargetPlatform
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}

object GenProgram {
  private val llibyInitDecl = {
    IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="lliby_init",
      arguments=Nil,
      attributes=Set(IrFunction.NoUnwind)
    )
  }
  
  private val llibyLaunchWorldDecl = {
    IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="_lliby_launch_world",
      arguments=List(IrFunction.Argument(
        // void (*entryPoint)(World *)
        PointerType(FunctionType(VoidType, List(PointerType(WorldValue.irType))))
      ))
    )
  }

  def resourceAsString(resourcePath : String) : String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    Source.fromInputStream(stream).mkString
  }

  def preludeIr : String = {
    List(
      resourceAsString("generated/cellTypes.ll"),
      resourceAsString("defines.ll")
    ) mkString "\n"
  }

  def apply(functions : Map[String, planner.PlannedFunction], targetPlatform : TargetPlatform, featureIdentifiers : Set[String]) : String = {
    val module = new IrModuleBuilder
    val plannedSymbols = functions.keySet

    val nextTbaaIndex = ct.CellType.nextTbaaIndex
    val typeGenerator = new TypeGenerator(module, targetPlatform, nextTbaaIndex)

    // Build each program-supplied function
    val functionGenerator = GenFunction(module, plannedSymbols, typeGenerator, targetPlatform)_ 

    for((nativeSymbol, plannedFunction) <- functions) {
      functionGenerator(nativeSymbol, plannedFunction)
    }
    
    // Build our main() glue to init the runtime and call our program
    val result = IrFunction.Result(IntegerType(32))
    val namedArguments = List(
      "argc" -> IrFunction.Argument(IntegerType(32)),
      "argv" -> IrFunction.Argument(PointerType(PointerType(IntegerType(8))))
    )

    val mainFunction = new IrFunctionBuilder(
      result=result,
      namedArguments=namedArguments,
      name="main") 

    val entryBlock = mainFunction.entryBlock

    // Initialize our runtime
    module.declareFunction(llibyInitDecl) 
    module.declareFunction(llibyLaunchWorldDecl)
    entryBlock.callDecl(None)(llibyInitDecl, Nil)
    
    // Call __llambda_exec through _lliby_launch_world
    // __llambda_exec must be defined by the planner
    val execValue = GenNamedEntryPoint(module)(LlambdaExecSignature, LlambdaExecSignature.nativeSymbol, plannedSymbols) 

    entryBlock.callDecl(None)(llibyLaunchWorldDecl, List(execValue), false)

    // Return 0
    // Scheme can only return non-zero exit codes using (exit)
    entryBlock.ret(IntegerConstant(IntegerType(32), 0))

    module.defineFunction(mainFunction)

    // Dump our type map
    typeGenerator.emitTypeMaps()

    // Convert our IR to one big string
    preludeIr + "\n" + module.toIr + "\n"
  }
}

