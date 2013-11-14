package llambda.codegen

import llambda.InternalCompilerErrorException

import llambda.planner.{step => ps}
import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}
import llambda.nfi

object GenConstant {
  protected case class Utf8Constant(irValue : IrConstant, byteLength : Int)
  
  protected def defineConstantData(module : IrModuleBuilder)(name : String, initializer  : IrConstant) : GlobalVariable = {
    val constantDataDef = IrGlobalVariableDef(
      name=name,
      initializer=initializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(constantDataDef)

    constantDataDef.variable
  }

  private def genUtf8Constant(module : IrModuleBuilder)(baseName : String, value : String) : Utf8Constant = {
    val innerConstantName = baseName + ".str"
    val innerConstantInitializer = StringConstant(value)

    val innerConstant = defineConstantData(module)(innerConstantName, innerConstantInitializer)

    Utf8Constant(
      irValue=ElementPointerConstant(IntegerType(8), innerConstant, List(0, 0)),
      // Don't include the NULL terminator
      byteLength=innerConstantInitializer.length - 1
    )
  }
  
  def genBoxedStringLike(module : IrModuleBuilder)(concreteType : bt.ConcreteBoxedType, baseName: String, value : String) : IrConstant = {
    // Build the inner string constant
    val utf8Constant = genUtf8Constant(module)(baseName, value)

    val boxedStringLikeName = baseName + ".box"

    val boxedStringLike = bt.BoxedStringLike.createConstant(
      typeId=IntegerConstant(IntegerType(16), concreteType.typeId),
      charLength=IntegerConstant(IntegerType(32), value.length),
      // Don't include the NULL terminator
      byteLength=IntegerConstant(IntegerType(32), utf8Constant.byteLength),
      utf8Data=utf8Constant.irValue)

    defineConstantData(module)(boxedStringLikeName, boxedStringLike)
  }

  def genBoxedBytevector(module : IrModuleBuilder)(elements : Seq[Short]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementIrs = elements.map(IntegerConstant(IntegerType(8), _))

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(IntegerType(8), elementIrs)
    
    val elementsValue = defineConstantData(module)(elementsName, elementsInitializer)

    val boxedBytevectorName = baseName + ".box"
    val boxedBytevector = bt.BoxedBytevector.createConstant(
      length=IntegerConstant(IntegerType(32), elements.length),
      data=ElementPointerConstant(IntegerType(8), elementsValue, List(0, 0)))

    defineConstantData(module)(boxedBytevectorName, boxedBytevector)
  }
  
  def genBoxedVector(module : IrModuleBuilder)(irElements : Seq[IrConstant]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeVector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(PointerType(bt.BoxedDatum.irType), irElements.toList)
    
    val elementsDef = defineConstantData(module)(elementsName, elementsInitializer)

    val boxedVectorName = baseName + ".box"

    val boxedVector = bt.BoxedVector.createConstant(
      length=IntegerConstant(IntegerType(32), irElements.length),
      elements=ElementPointerConstant(PointerType(bt.BoxedDatum.irType), elementsDef, List(0, 0)))

    defineConstantData(module)(boxedVectorName, boxedVector)
  }

  def apply(state : GenerationState)(storeStep : ps.StoreConstant) : IrConstant = storeStep match {
    case ps.StoreBoxedString(_, value) =>
      val baseName = state.module.nameSource.allocate("schemeString")
      val stringLike = genBoxedStringLike(state.module)(bt.BoxedString, baseName, value)

      BitcastToConstant(stringLike, PointerType(bt.BoxedString.irType))

    case ps.StoreBoxedSymbol(_, value) =>
      val baseName = state.module.nameSource.allocate("schemeSymbol")
      val stringLike = genBoxedStringLike(state.module)(bt.BoxedSymbol, baseName, value)
      
      BitcastToConstant(stringLike, PointerType(bt.BoxedSymbol.irType))

    case ps.StoreNativeUtf8String(_, value) =>
      val utf8StringName = state.module.nameSource.allocate("schemeBareCString")

      genUtf8Constant(state.module)(utf8StringName, value).irValue

    case ps.StoreBoxedExactInteger(_, value) =>
      val boxedIntName = state.module.nameSource.allocate("schemeExactInteger")

      val boxedInt = bt.BoxedExactInteger.createConstant(
        value=IntegerConstant(IntegerType(64), value)
      )

      defineConstantData(state.module)(boxedIntName, boxedInt)
    
    case ps.StoreBoxedInexactRational(_, value) =>
      val boxedRationalName = state.module.nameSource.allocate("schemeInexactRational")

      val boxedRational = bt.BoxedInexactRational.createConstant(
        value=DoubleConstant(value)
      )

      defineConstantData(state.module)(boxedRationalName, boxedRational)

    case ps.StoreBoxedBoolean(_, true) =>
      GlobalDefines.trueIrValue

    case ps.StoreBoxedBoolean(_, false) =>
      GlobalDefines.falseIrValue
    
    case ps.StoreBoxedCharacter(_, value) =>
      val boxedCharName = state.module.nameSource.allocate("schemeCharacter")

      val boxedChar = bt.BoxedCharacter.createConstant(
        unicodeChar=IntegerConstant(IntegerType(32), value)
      )

      defineConstantData(state.module)(boxedCharName, boxedChar)
    
    case ps.StoreBoxedBytevector(_, elements) =>
      genBoxedBytevector(state.module)(elements)
    
    case ps.StoreBoxedVector(_, elementTemps) =>
      val elementIrs = elementTemps.map { elementTemp =>
        state.liveTemps(elementTemp) match {
          case constant : IrConstant => constant
          case other =>
            throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant car: ${other}")
        }
      }

      genBoxedVector(state.module)(elementIrs)

    case ps.StoreBoxedPair(_, carTemp, cdrTemp) =>
      val boxedPairName = state.module.nameSource.allocate("schemeInexactPair")
      val carIrConstant = state.liveTemps(carTemp) match {
        case constant : IrConstant => constant
        case other =>
          throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant car: ${other}")
      }

      val cdrIrConstant = state.liveTemps(cdrTemp) match {
        case constant : IrConstant => constant
        case other =>
          throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant cdr: ${other}")
      }

      val boxedPair = bt.BoxedPair.createConstant(
        car=carIrConstant,
        cdr=cdrIrConstant
      )

      defineConstantData(state.module)(boxedPairName, boxedPair)

    case ps.StoreBoxedUnspecific(_) =>
      GlobalDefines.unspecificIrValue

    case ps.StoreBoxedEmptyList(_) =>
      GlobalDefines.emptyListIrValue

    case ps.StoreNativeInteger(_, value, bits) =>
      IntegerConstant(IntegerType(bits), value)
    
    case ps.StoreNativeFloat(_, value, fpType) if fpType == nfi.Double =>
      DoubleConstant(value)

    case ps.StoreNativeFloat(_, value, fpType) if fpType == nfi.Float =>
      FloatConstant(value.toFloat)
  }
}

