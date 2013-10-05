package llambda.codegen

import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}
import llambda.nfi

object LiveValueToNative {
  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(liveValue : LiveValue, targetType : nfi.NativeType) : IrValue ={
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        // Do we already have a boxed value?
        for(boxedValue <- liveValue.boxedValue) {
          val expectedConcreteTypes = ((expectedType :: expectedType.subtypes) collect {
            case concrete : bt.ConcreteBoxedType => concrete
          }).toSet

          if (liveValue.possibleTypes.subsetOf(expectedConcreteTypes)) {
            // No type conversion needed - just bitcast
            return expectedType.genPointerBitcast(block)(boxedValue)
          }
        }

        GenConstantBoxedValue(module, block)(liveValue, expectedType)
    }
  }
}
