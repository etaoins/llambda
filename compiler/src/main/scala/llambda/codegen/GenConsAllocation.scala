package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.codegen.llvmir.IrFunction._

object GenConsAllocation {
  private val consType = UserDefinedType("cons")
  private val consPointerType = PointerType(consType)
  
  // Note these are pointers-to-pointers
  private val llibyAllocStart = GlobalVariable("_lliby_alloc_start", PointerType(consPointerType))
  private val llibyAllocEnd = GlobalVariable("_lliby_alloc_end", PointerType(consPointerType))

  private val llibyAllocCons = IrFunctionDecl(
    result=Result(consPointerType),
    name="_lliby_alloc_cons",
    arguments=List(Argument(IntegerType(64))),
    attributes=Set(NoUnwind)
  )

  protected class ConsAllocation(basePointer : IrValue, count : Int) {
    def genTypedPointer(state : GenerationState)(index : Int, asType : bt.ConcreteBoxedType) : IrValue = {
      val block = state.currentBlock

      if (index >= count) {
        throw new InternalCompilerErrorException("Attempted to access cons past end of allocation")
      }

      // We have to do this on %cons because the target type might be the wrong size
      val indexValue = IntegerConstant(IntegerType(32), index)
      val consPointer = block.getelementptr(s"cons${index}Ptr")(consType, basePointer, List(indexValue))

      // Cast to the destination type
      val pointerName = s"cons${index}${asType.name.capitalize}Ptr"
      val typedPointer = block.bitcastTo(pointerName)(consPointer, PointerType(bt.BoxedPair.irType))
      
      // Set its type
      val typeIdPointer = asType.genPointerToTypeId(block)(typedPointer)
      block.store(IntegerConstant(IntegerType(16), asType.typeId), typeIdPointer)

      // Return the typed pointer
      typedPointer
    }
  }

  def apply(initialState : GenerationState)(count : Int) : (GenerationState, ConsAllocation)  = {
    val startBlock = initialState.currentBlock

    if (count == 0) {
      val allocation = new ConsAllocation(NullPointerConstant(consPointerType), 0)
      return (initialState, allocation)
    }

    initialState.module.unlessDeclared(llibyAllocCons) {
      initialState.module.declareFunction(llibyAllocCons)
    }

    startBlock.comment(s"allocating ${count} cons")

    // We need this a few times
    val allocCountValue = IntegerConstant(IntegerType(64), count)

    // Load the pointer to our allocation start
    // This is our allocation unless we run out of memory
    val directAllocValue = startBlock.load("directAlloc")(llibyAllocStart)
    
    // Load the pointer to our allocation end value
    val allocEndValue = startBlock.load("allocEnd")(llibyAllocEnd)

    // Add our allocation count on to our allocation
    val newAllocStartValue = startBlock.getelementptr("newAllocStart")(consType, directAllocValue, List(allocCountValue))

    // Create our child blocks for the upcoming branch
    val directSuccessBlock = startBlock.startChildBlock("directSuccess")
    val collectGarbageBlock = startBlock.startChildBlock("collectGarbage")
    val allocFinishedBlock = startBlock.startChildBlock("allocFinished")

    // See if we ran out of space
    val directSucceededPred = startBlock.icmp("directSucceeded")(ComparisonCond.LessThanEqual, Some(false), newAllocStartValue, allocEndValue)
    startBlock.condBranch(directSucceededPred, directSuccessBlock, collectGarbageBlock)

    // In the direct alloc block store our new start pointer
    directSuccessBlock.store(newAllocStartValue, llibyAllocStart)
    directSuccessBlock.uncondBranch(allocFinishedBlock)

    // In the garage collection block call out to the runtime
    val Some(runtimeAllocValue) = collectGarbageBlock.callDecl(Some("runtimeAlloc"))(llibyAllocCons, List(allocCountValue))
    collectGarbageBlock.uncondBranch(allocFinishedBlock)

    // Phi the two values together
    val allocResultValue = allocFinishedBlock.phi("allocResult")(
      PhiSource(directAllocValue, directSuccessBlock),
      PhiSource(runtimeAllocValue, collectGarbageBlock)
    )

    val allocation = new ConsAllocation(allocResultValue, count)
    (initialState.copy(currentBlock=allocFinishedBlock), allocation)
  }
}
