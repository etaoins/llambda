package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

class BoxedLiveValue(boxedType : bt.BoxedType, boxedValue : IrValue) extends LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType] =
    (boxedType :: boxedType.subtypes).collect({
        case concrete : bt.ConcreteBoxedType => concrete
    }).toSet
  
  protected def genUnboxing(state : GenerationState)(targetType : nfi.NativeType) : Option[IrValue] = 
    None

  protected def genCastToBoxed(initialState : GenerationState)(targetType : bt.BoxedType) : Option[(GenerationState, IrValue)] = {
    if (targetType.isTypeOrSupertypeOf(boxedType)) {
      // This doesn't require any type assertions
      val supertypeValue = targetType.genPointerBitcast(initialState.currentBlock)(boxedValue)

      Some((initialState, supertypeValue))
    }
    else if (targetType.isTypeOrSubtypeOf(boxedType)) {
      // We need to build a type assertion
      val successBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastSuccess") 
      val failBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastFail") 

      // Do the actual check
      targetType.genTypeCheck(initialState.currentBlock)(boxedValue, successBlock, failBlock)
    
      // Generate the fail branch
      val errorName = s"subcastFrom${boxedType.name.capitalize}To${targetType.name.capitalize}"
      val errorMessage = s"Runtime cast from '${boxedType.name}' to subtype '${targetType.name}' failed" 
      GenFatalError(initialState.module, failBlock)(errorName, errorMessage)

      // Continue building on the success block
      val subtypeValue = targetType.genPointerBitcast(successBlock)(boxedValue)
      Some((initialState.copy(currentBlock=successBlock), subtypeValue))
    }
    else {
      None
    }
  }

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        genCastToBoxed(state)(expectedType)
    }
  }
}
