package llambda.codegen

import llambda._
import llambda.codegen.llvmir._
import llambda.planner.{step => ps}

import scala.io.Source

object GenProgram {
  private val llibyInitDecl = {
    IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="lliby_init",
      arguments=Nil,
      attributes=Set(IrFunction.NoUnwind)
    )
  }

  def resourceAsString(resourcePath : String) : String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    io.Source.fromInputStream(stream).mkString
  }

  def preludeIr : String = {
    List(
      resourceAsString("generated/boxedTypes.ll"),
      resourceAsString("defines.ll")
    ) mkString "\n"
  }

  def apply(functions : Map[String, planner.PlannedFunction]) : String = {
    val module = new llvmir.IrModuleBuilder
    val plannedSymbols = functions.keySet

    // Build each program-supplied function
    for((nativeSymbol, plannedFunction) <- functions) {
      val irSignature = NativeSignatureToIr(plannedFunction.signature)

      val argumentNames = plannedFunction.namedArguments.map(_._1)
      val namedIrArguments = argumentNames.zip(irSignature.arguments)
      
      // This function does not need to be externally accessible
      // This allows LLVM to more aggressively optimize and reduces the chance
      // of symbol conflicts with other objects
      val generatedFunction = new IrFunctionBuilder(
        result=irSignature.result,
        namedArguments=namedIrArguments,
        name=nativeSymbol,
        linkage=Linkage.Internal,
        attributes=Set(IrFunction.NoUnwind)) 

      // Create a blank generation state with just our args
      val argTemps = (plannedFunction.namedArguments map { case (name, tempValue) =>
        (tempValue, generatedFunction.argumentValues(name))
      }).toMap

      val startState = GenerationState(
        module=module,
        currentBlock=generatedFunction.entryBlock,
        liveTemps=argTemps)

      // Generate our steps
      GenPlanSteps(startState, plannedSymbols)(plannedFunction.steps)

      module.defineFunction(generatedFunction)
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
    entryBlock.callDecl(None)(llibyInitDecl, Nil)
    
    // Call __llambda_exec
    // This must be defined by the planner
    val execIrSignature = NativeSignatureToIr(LlambdaExecSignature)
    val execValue = GenNamedEntryPoint(module)(LlambdaExecSignature, LlambdaExecSignature.nativeSymbol, plannedSymbols) 

    entryBlock.call(None)(execIrSignature, execValue, Nil, false)

    // Return 0
    // Scheme can only return non-zero exit codes using (exit)
    entryBlock.ret(IntegerConstant(IntegerType(32), 0))

    module.defineFunction(mainFunction)

    // Convert our IR to one big string
    preludeIr + "\n" + module.toIr + "\n"
  }
}

