package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

abstract class LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)]

  def genTruthyPredicate(state : GenerationState) : IrValue

  def toRequiredNativeType(state : GenerationState)(targetType : nfi.NativeType) : (GenerationState, IrValue) = {
    toNativeType(state)(targetType) getOrElse {
      throw new ImpossibleTypeConversionException("Unable to convert " + this.toString + " to " + targetType)
    }
  }

  def genPhiWith(ourInitialState : GenerationState, theirInitialState : GenerationState)(theirValue : LiveValue) : (GenerationState, LiveValue) = {
    // This can be potentially be tragically expensive
    // Subclasses can override this to do saner things
    val (ourState, ourBoxedValue) = toRequiredNativeType(ourInitialState)(nfi.BoxedValue(bt.BoxedDatum))
    val (theirState, theirBoxedValue) = theirValue.toRequiredNativeType(theirInitialState)(nfi.BoxedValue(bt.BoxedDatum))

    // Figure out the possible types of our phi'ed value
    val mergedPossibleTypes = possibleTypes ++ theirValue.possibleTypes

    // Jump to the phi block
    val phiBlock = ourState.currentBlock.startChildBlock("valuePhi") 
    ourState.currentBlock.uncondBranch(phiBlock)
    theirState.currentBlock.uncondBranch(phiBlock)

    val resultIrValue = phiBlock.phi("phiResult")(
      PhiSource(value=ourBoxedValue, block=ourState.currentBlock),
      PhiSource(value=theirBoxedValue, block=theirState.currentBlock)
    )
    
    val resultValue = new BoxedLiveValue(mergedPossibleTypes, resultIrValue) 

    // XXX: Which state should we use here as a template?
    (ourState.copy(currentBlock=phiBlock), resultValue)
  }
}

