package io.llambda.compiler.codegen
import io.llambda

import scala.io.Codec

import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object GenConstant {
  private def arrayElementsForIrType(irType : IrType) : Int= irType match {
    case ArrayType(elements, _) =>
      elements

    case _ =>
      throw new InternalCompilerErrorException("Expected cell member to be array")
  }

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

  private def genHeapUtf8Constant(module : IrModuleBuilder)(baseName : String, utf8Data : Array[Byte]) : IrConstant = {
    val innerConstantName = baseName + ".str"
    // Add a single NULL terminator
    val innerConstantInitializer = StringConstant(utf8Data :+ 0.toByte)

    val innerConstant = defineConstantData(module)(innerConstantName, innerConstantInitializer)
    ElementPointerConstant(IntegerType(8), innerConstant, List(0, 0))
  }
  
  private def genInlineUtf8Constant(module : IrModuleBuilder)(utf8Data : Array[Byte], constantLength : Int) : IrConstant = {
    val padByteCount = constantLength - utf8Data.length

    // Pad this string with 0s
    val terminatedUtf8Data = utf8Data ++ Array.fill(padByteCount)(0.toByte)

    // Return a direct constant
    StringConstant(terminatedUtf8Data)
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

  def genEmptyClosure(module : IrModuleBuilder, typeGenerator : TypeGenerator)(entryPoint : IrConstant) : IrConstant = {
    val procCellName = module.nameSource.allocate("schemeProcedure")

    // Find the class ID for the empty closure type
    val generatedType = typeGenerator(vt.EmptyClosureType)
      
    val procCell = ct.ProcedureCell.createConstant(
      entryPoint=entryPoint,
      recordClassId=generatedType.classId,
      // This can be anything as it's unused. Dynamic record cells leave this
      // uninitialized to be instruction thrifty but we have to supply a value
      // here
      recordData=NullPointerConstant(ct.ProcedureCell.recordDataIrType)
    )

    defineConstantData(module)(procCellName, procCell)
  }

  def genStringCell(module : IrModuleBuilder)(value : String) : IrConstant = {
    val baseName = module.nameSource.allocate("schemeString")
    val stringCellName = baseName + ".cell"

    val utf8Data = Codec.toUTF8(value)
    val inlineUtf8Bytes = arrayElementsForIrType(ct.InlineStringCell.inlineDataIrType)

    val stringCell = if (utf8Data.length < inlineUtf8Bytes) {
      // We can do this inline
      val utf8Constant = genInlineUtf8Constant(module)(utf8Data, inlineUtf8Bytes)

      ct.InlineStringCell.createConstant(
        inlineData=utf8Constant,
        allocSlackBytes=0,
        charLength=value.length,
        byteLength=utf8Data.length
      )
    }
    else {
      // We have to put this on the heap
      val utf8Constant = genHeapUtf8Constant(module)(baseName, utf8Data)

      ct.HeapStringCell.createConstant(
        heapData=utf8Constant,
        allocSlackBytes=0,
        charLength=value.length,
        byteLength=utf8Data.length
      )
    }

    // The rest of the compiler assumes this is just a %string*
    BitcastToConstant(
      defineConstantData(module)(stringCellName, stringCell),
      PointerType(ct.StringCell.irType)
    )
  }
  
  def genSymbolCell(module : IrModuleBuilder)(value : String) : IrConstant = {
    val baseName = module.nameSource.allocate("schemeSymbol")
    val symbolCellName = baseName + ".cell"

    val utf8Data = Codec.toUTF8(value)
    val inlineUtf8Bytes = arrayElementsForIrType(ct.InlineSymbolCell.inlineDataIrType)

    val symbolCell = if (utf8Data.length < inlineUtf8Bytes) {
      // We can do this inline
      val utf8Constant = genInlineUtf8Constant(module)(utf8Data, inlineUtf8Bytes)

      ct.InlineSymbolCell.createConstant(
        inlineData=utf8Constant,
        charLength=value.length,
        byteLength=utf8Data.length
      )
    }
    else {
      // We have to put this on the heap
      val utf8Constant = genHeapUtf8Constant(module)(baseName, utf8Data)

      ct.HeapSymbolCell.createConstant(
        heapData=utf8Constant,
        charLength=value.length,
        byteLength=utf8Data.length
      )
    }

    // The rest of the compiler assumes this is just a %symbol*
    BitcastToConstant(
      defineConstantData(module)(symbolCellName, symbolCell),
      PointerType(ct.SymbolCell.irType)
    )
  }

  def apply(state : GenerationState, typeGenerator : TypeGenerator)(storeStep : ps.StoreConstant) : IrConstant = storeStep match {
    case ps.StoreStringCell(_, value) =>
      genStringCell(state.module)(value)

    case ps.StoreSymbolCell(_, value) =>
      genSymbolCell(state.module)(value)

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
        unicodeChar=value
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

    case ps.StoreUnitCell(_) =>
      GlobalDefines.unitIrValue

    case ps.StoreEmptyListCell(_) =>
      GlobalDefines.emptyListIrValue

    case ps.StoreEmptyClosure(_, entryPointTemp) =>
      val entryPointConstant = state.liveTemps(entryPointTemp) match {
        case constant : IrConstant => constant
        case other =>
          throw new InternalCompilerErrorException(s"Attempted to create constant closure with non-constant entry point: ${other}")
      }

      genEmptyClosure(state.module, typeGenerator)(entryPointConstant)

    case ps.StoreNativeInteger(_, value, bits) =>
      IntegerConstant(IntegerType(bits), value)
    
    case ps.StoreNativeFloat(_, value, fpType) if fpType == vt.Double =>
      DoubleConstant(value)

    case ps.StoreNativeFloat(_, value, fpType) if fpType == vt.Float =>
      FloatConstant(value.toFloat)
  }
}

