package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._
import llambda.nfi

abstract class ConstantLiveStringLike(module : IrModuleBuilder)(constantValue : String, concreteType : bt.ConcreteBoxedType) extends ConstantLiveValue(concreteType) {
  private val baseName = module.nameSource.allocate("scheme" + concreteType.name.capitalize)

  protected case class Utf8Constant(
    irValue : IrConstant,
    byteLength : Int
  )

  protected def genUtf8Constant() : Utf8Constant = {
    val innerConstantName = baseName + ".str"
    val innerConstantInitializer = StringConstant(constantValue)

    val innerConstantDef = IrGlobalVariableDef(
      name=innerConstantName,
      initializer=innerConstantInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(innerConstantDef)

    Utf8Constant(
      irValue=ElementPointerConstant(IntegerType(8), innerConstantDef.variable, List(0, 0)),
      // Don't include the NULL terminator
      byteLength=innerConstantInitializer.length - 1
    )
  }

  def genBoxedConstant() : IrConstant = {
    // Build the inner string constant
    val utf8Constant = genUtf8Constant()

    val boxedStringLikeName = baseName + ".box"
    val boxedStringLike = bt.BoxedStringLike.createConstant(
      typeId=IntegerConstant(IntegerType(16), concreteType.typeId),
      charLength=IntegerConstant(IntegerType(32), constantValue.length),
      // Don't include the NULL terminator
      byteLength=IntegerConstant(IntegerType(32), utf8Constant.byteLength),
      utf8Data=utf8Constant.irValue)

    declareBoxedConstant(module)(boxedStringLikeName, boxedStringLike)
  }
}
