package llambda.codegen

import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}
import llambda.nfi

object LiveValueToNative {
  

  private def genConstantBoxedString(module : IrModuleBuilder, block : IrBlockBuilder)(content : String) : IrValue = {
    // XXX: Can we get a better name here?
    val baseName = module.nameSource.allocate("schemeString")
    
    // Build the inner string constant
    val innerConstantName = baseName + ".str"
    val innerConstantInitializer = StringConstant(content)

    val innerConstantDef = IrGlobalVariableDef(
      name=innerConstantName,
      initializer=innerConstantInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(innerConstantDef)

    val boxedStringName = baseName + ".box"
    val boxedStringInitializer = bt.BoxedString.createConstant(
      charLength=IntegerConstant(IntegerType(32), content.length),
      // Don't include the NULL terminator
      byteLength=IntegerConstant(IntegerType(32), innerConstantInitializer.length - 1),
      utf8Data=ElementPointerConstant(IntegerType(8), innerConstantDef.variable, List(0, 0)))

    val boxedStringDef = IrGlobalVariableDef(
      name=boxedStringName,
      initializer=boxedStringInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(boxedStringDef)

    boxedStringDef.variable
  }

  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(liveValue : LiveValue, targetType : nfi.NativeType) : IrValue ={
    (liveValue, targetType) match {
      case (LiveString(Some(constantStr), _, _), nfi.BoxedValue(expectedType)) =>
        if (!bt.BoxedString.isTypeOrSubtypeOf(expectedType)) {
          // Not possible
          GenFatalError(module, block)("cantConvertStringTo${expectedType.name.capitalize}", s"Can't convert string to boxed type of ${expectedType.name}")
          LiveUnspecific.singletonValue
        }
        else {
          val irStringValue = genConstantBoxedString(module, block)(constantStr)

          // Cast to the expected boxed type
          expectedType.genPointerBitcast(block)(irStringValue)
        }
    }
  }
}
