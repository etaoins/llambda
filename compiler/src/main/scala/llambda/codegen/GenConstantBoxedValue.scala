package llambda.codegen

import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}
import llambda.nfi

object GenConstantBoxedValue {
  private def declareBoxedConstant(module : IrModuleBuilder)(name : String, constantValue  : IrConstant) : IrConstant = {
    val boxedConstantDef = IrGlobalVariableDef(
      name=name,
      initializer=constantValue,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(boxedConstantDef)

    boxedConstantDef.variable
  }

  private def genConstantBoxedStringLike(module : IrModuleBuilder, block : IrBlockBuilder)(content : String, targetType : bt.ConcreteBoxedType) : IrConstant = {
    // XXX: Can we get a better name here?
    val baseName = module.nameSource.allocate("scheme" + targetType.name.capitalize)
    
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

    val boxedStringLikeName = baseName + ".box"
    val boxedStringLike = bt.BoxedStringLike.createConstant(
      typeId=IntegerConstant(IntegerType(16), targetType.typeId),
      charLength=IntegerConstant(IntegerType(32), content.length),
      // Don't include the NULL terminator
      byteLength=IntegerConstant(IntegerType(32), innerConstantInitializer.length - 1),
      utf8Data=ElementPointerConstant(IntegerType(8), innerConstantDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedStringLikeName, boxedStringLike)
  }
  
  private def genConstantBoxedExactInteger(module : IrModuleBuilder, block : IrBlockBuilder)(value : Int) : IrConstant = {
    // XXX: Can we get a better name here?
    val boxedIntName = module.nameSource.allocate("schemeExactInteger")

    val boxedInt = bt.BoxedExactInteger.createConstant(
      value=IntegerConstant(IntegerType(64), value)
    )

    declareBoxedConstant(module)(boxedIntName, boxedInt)
  }
  
  private def genConstantBoxedInexactRational(module : IrModuleBuilder, block : IrBlockBuilder)(value : Double) : IrConstant = {
    // XXX: Can we get a better name here?
    val boxedIntName = module.nameSource.allocate("schemeInexactRational")

    val boxedInt = bt.BoxedInexactRational.createConstant(
      value=DoubleConstant(value)
    )

    declareBoxedConstant(module)(boxedIntName, boxedInt)
  }

  private def genConstantBoxedBoolean(module : IrModuleBuilder, block : IrBlockBuilder)(value : Boolean) : IrConstant = {
    if (value) {
      GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
    }
    else {
      GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))
    }
  }
  
  private def genConstantBoxedCharacter(module : IrModuleBuilder, block : IrBlockBuilder)(value : Char) : IrConstant = {
    val boxedCharName = module.nameSource.allocate("schemeCharacter")

    val boxedChar = bt.BoxedCharacter.createConstant(
      unicodeChar=IntegerConstant(IntegerType(32), value)
    )

    declareBoxedConstant(module)(boxedCharName, boxedChar)
  }
  
  private def genConstantBoxedPair(module : IrModuleBuilder, block : IrBlockBuilder)(liveCar : LiveValue, liveCdr : LiveValue) : IrConstant = {
    val irCar = GenConstantBoxedValue(module, block)(liveCar, bt.BoxedDatum)
    val irCdr = GenConstantBoxedValue(module, block)(liveCdr, bt.BoxedDatum)
    
    val boxedPairName = module.nameSource.allocate("schemePair")

    val boxedPair = bt.BoxedPair.createConstant(
      car=irCar,
      cdr=irCdr
    )
    
    declareBoxedConstant(module)(boxedPairName, boxedPair)
  }
  
  private def genConstantVector(module : IrModuleBuilder, block : IrBlockBuilder)(liveElements : Vector[LiveValue]) : IrConstant = {
    val irElements = liveElements.map(GenConstantBoxedValue(module, block)(_, bt.BoxedDatum))
    
    // Make our elements
    val baseName = module.nameSource.allocate("schemeVector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(PointerType(bt.BoxedDatum.irType), irElements.toList)
    
    val elementsDef = IrGlobalVariableDef(
      name=elementsName,
      initializer=elementsInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(elementsDef)

    val boxedVectorName = baseName + ".box"

    val boxedVector = bt.BoxedVector.createConstant(
      length=IntegerConstant(IntegerType(32), irElements.length),
      elements=ElementPointerConstant(PointerType(bt.BoxedDatum.irType), elementsDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedVectorName, boxedVector)
  }
  
  private def genConstantBytevector(module : IrModuleBuilder, block : IrBlockBuilder)(elements : Vector[Int]) : IrConstant = {
    val irElements = elements.map(IntegerConstant(IntegerType(8), _))
    
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(IntegerType(8), irElements.toList)
    
    val elementsDef = IrGlobalVariableDef(
      name=elementsName,
      initializer=elementsInitializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(elementsDef)

    val boxedBytevectorName = baseName + ".box"

    val boxedBytevector = bt.BoxedBytevector.createConstant(
      length=IntegerConstant(IntegerType(32), irElements.length),
      data=ElementPointerConstant(IntegerType(8), elementsDef.variable, List(0, 0)))

    declareBoxedConstant(module)(boxedBytevectorName, boxedBytevector)
  }

  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(liveValue : LiveValue, expectedType : bt.BoxedType) : IrConstant = {
    if (!liveValue.possibleTypes.exists(_.isTypeOrSubtypeOf(expectedType))) {
      GenFatalError(module, block)("cantConvertTo${expectedType.name.capitalize}", s"Cannot convert value to boxed type of ${expectedType.name}")
      return LiveUnspecific.singletonValue
    }

    val uncastIrValue = liveValue match {
      case LiveString(Some(constantStr), _, _) =>
        genConstantBoxedStringLike(module, block)(constantStr, bt.BoxedString)
      
      case LiveSymbol(Some(constantStr), _, _) =>
        genConstantBoxedStringLike(module, block)(constantStr, bt.BoxedSymbol)

      case LiveExactInteger(Some(constantInt), _) =>
        genConstantBoxedExactInteger(module, block)(constantInt)
      
      case LiveInexactRational(Some(constantDouble), _) =>
        genConstantBoxedInexactRational(module, block)(constantDouble)
      
      case LiveBoolean(Some(constantBool), _) =>
        genConstantBoxedBoolean(module, block)(constantBool)
      
      case LiveCharacter(Some(constantChar), _) =>
        genConstantBoxedCharacter(module, block)(constantChar)
      
      case LivePair(Some((constantCar, constantCdr)), _) =>
        genConstantBoxedPair(module, block)(constantCar, constantCdr)

      case LiveVector(Some(constantElements), _) =>
        genConstantVector(module, block)(constantElements)
      
      case LiveBytevector(Some(constantElements), _) =>
        genConstantBytevector(module, block)(constantElements)

      case LiveUnspecific =>
        LiveUnspecific.singletonValue
      
      case LiveEmptyList =>
        LiveEmptyList.singletonValue
    }
    
    // Cast to the expected boxed type
    BitcastToConstant(uncastIrValue, PointerType(expectedType.irType))
  }
}
