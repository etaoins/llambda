package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object GenConstant {
  protected case class Utf8Constant(irValue : IrConstant, byteLength : Int)
  
  protected def defineConstantData(module : IrModuleBuilder)(name : String, initializer  : IrConstant) : GlobalVariable = {
    val constantDataDef = IrGlobalVariableDef(
      name=name,
      initializer=initializer,
      linkage=Linkage.Private,
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
  
  def genStringLikeCell(module : IrModuleBuilder)(concreteType : ct.ConcreteCellType, baseName: String, value : String) : IrConstant = {
    // Build the inner string constant
    val utf8Constant = genUtf8Constant(module)(baseName, value)

    val stringLikeCellName = baseName + ".cell"

    val stringLikeCell = ct.StringLikeCell.createConstant(
      typeId=concreteType.typeId,
      charLength=value.length,
      // Don't include the NULL terminator
      byteLength=utf8Constant.byteLength,
      utf8Data=utf8Constant.irValue)

    defineConstantData(module)(stringLikeCellName, stringLikeCell)
  }

  def genBytevectorCell(module : IrModuleBuilder)(elements : Seq[Short]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementIrs = elements.map(IntegerConstant(IntegerType(8), _))

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(IntegerType(8), elementIrs)
    
    val elementsValue = defineConstantData(module)(elementsName, elementsInitializer)

    val bytevectorCellName = baseName + ".cell"
    val bytevectorCell = ct.BytevectorCell.createConstant(
      length=elements.length,
      data=ElementPointerConstant(IntegerType(8), elementsValue, List(0, 0)))

    defineConstantData(module)(bytevectorCellName, bytevectorCell)
  }
  
  def genVectorCell(module : IrModuleBuilder)(irElements : Seq[IrConstant]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeVector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(PointerType(ct.DatumCell.irType), irElements.toList)
    
    val elementsDef = defineConstantData(module)(elementsName, elementsInitializer)

    val vectorCellName = baseName + ".cell"

    val vectorCell = ct.VectorCell.createConstant(
      length=irElements.length,
      elements=ElementPointerConstant(PointerType(ct.DatumCell.irType), elementsDef, List(0, 0)))

    defineConstantData(module)(vectorCellName, vectorCell)
  }

  def apply(state : GenerationState)(storeStep : ps.StoreConstant) : IrConstant = storeStep match {
    case ps.StoreStringCell(_, value) =>
      val baseName = state.module.nameSource.allocate("schemeString")
      val stringLike = genStringLikeCell(state.module)(ct.StringCell, baseName, value)

      BitcastToConstant(stringLike, PointerType(ct.StringCell.irType))

    case ps.StoreSymbolCell(_, value) =>
      val baseName = state.module.nameSource.allocate("schemeSymbol")
      val stringLike = genStringLikeCell(state.module)(ct.SymbolCell, baseName, value)
      
      BitcastToConstant(stringLike, PointerType(ct.SymbolCell.irType))

    case ps.StoreNativeUtf8String(_, value) =>
      val utf8StringName = state.module.nameSource.allocate("schemeBareCString")

      genUtf8Constant(state.module)(utf8StringName, value).irValue

    case ps.StoreExactIntegerCell(_, value) =>
      val intCellName = state.module.nameSource.allocate("schemeExactInteger")

      val intCell = ct.ExactIntegerCell.createConstant(
        value=value
      )

      defineConstantData(state.module)(intCellName, intCell)
    
    case ps.StoreInexactRationalCell(_, value) =>
      val rationalCellName = state.module.nameSource.allocate("schemeInexactRational")

      val rationalCell = ct.InexactRationalCell.createConstant(
        value=DoubleConstant(value)
      )

      defineConstantData(state.module)(rationalCellName, rationalCell)

    case ps.StoreBooleanCell(_, true) =>
      GlobalDefines.trueIrValue

    case ps.StoreBooleanCell(_, false) =>
      GlobalDefines.falseIrValue
    
    case ps.StoreCharacterCell(_, value) =>
      val charCellName = state.module.nameSource.allocate("schemeCharacter")

      val charCell = ct.CharacterCell.createConstant(
        unicodeChar=IntegerConstant(IntegerType(32), value)
      )

      defineConstantData(state.module)(charCellName, charCell)
    
    case ps.StoreBytevectorCell(_, elements) =>
      genBytevectorCell(state.module)(elements)
    
    case ps.StoreVectorCell(_, elementTemps) =>
      val elementIrs = elementTemps.map { elementTemp =>
        state.liveTemps(elementTemp) match {
          case constant : IrConstant => constant
          case other =>
            throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant car: ${other}")
        }
      }

      genVectorCell(state.module)(elementIrs)

    case ps.StorePairCell(_, carTemp, cdrTemp) =>
      val pairCellName = state.module.nameSource.allocate("schemeInexactPair")
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

      val pairCell = ct.PairCell.createConstant(
        car=carIrConstant,
        cdr=cdrIrConstant
      )

      defineConstantData(state.module)(pairCellName, pairCell)

    case ps.StoreUnspecificCell(_) =>
      GlobalDefines.unspecificIrValue

    case ps.StoreEmptyListCell(_) =>
      GlobalDefines.emptyListIrValue

    case ps.StoreNativeInteger(_, value, bits) =>
      IntegerConstant(IntegerType(bits), value)
    
    case ps.StoreNativeFloat(_, value, fpType) if fpType == vt.Double =>
      DoubleConstant(value)

    case ps.StoreNativeFloat(_, value, fpType) if fpType == vt.Float =>
      FloatConstant(value.toFloat)
  }
}

