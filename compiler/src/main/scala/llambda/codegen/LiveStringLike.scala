package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

abstract class ConstantLiveStringLike(constantValue : String, concreteType : bt.ConcreteBoxedType) extends ConstantLiveValue(concreteType) {
  def genBoxedConstant(module : IrModuleBuilder) : IrConstant = {
    val baseName = module.nameSource.allocate("scheme" + concreteType.name.capitalize)
    
    // Build the inner string constant
    val innerConstantName = baseName + ".str"
    val innerConstantInitializer = StringConstant(constantValue)

    val innerConstantDef = IrGlobalVariableDef(
      name=innerConstantName,
      initializer=innerConstantInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(innerConstantDef)

    val boxedStringLikeName = baseName + ".box"
    val boxedStringLike = bt.BoxedStringLike.createConstant(
      typeId=IntegerConstant(IntegerType(16), concreteType.typeId),
      charLength=IntegerConstant(IntegerType(32), constantValue.length),
      // Don't include the NULL terminator
      byteLength=IntegerConstant(IntegerType(32), innerConstantInitializer.length - 1),
      utf8Data=ElementPointerConstant(IntegerType(8), innerConstantDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedStringLikeName, boxedStringLike)
  }
}
