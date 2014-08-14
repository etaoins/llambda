package io.llambda.compiler.codegen
import io.llambda

import scala.io.Codec

import scala.collection.mutable

import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}


class ConstantGenerator(typeGenerator : TypeGenerator) {
  /* Caches of constants indexed by their value 
   *
   * This is ensure proper Scheme semantics are enforced with the lazily value instantiation the planner does. For
   * example, the following code:
   * (define x '(1 2 3))
   * (eq? x x)
   * 
   * Will actually plan the list for 'x' once for each argument. This breaks the Scheme semantics as (eq?) would
   * return false there
   */

  private val stringCache = new mutable.HashMap[String, IrConstant]
  private val symbolCache = new mutable.HashMap[String, IrConstant]
  private val exactIntegerCache = new mutable.HashMap[Long, IrConstant]
  private var nanCache : Option[IrConstant] = None
  private val flonumCache = new mutable.HashMap[Double, IrConstant]
  private val characterCache = new mutable.HashMap[Char, IrConstant]
  private val bytevectorCache = new mutable.HashMap[Vector[Short], IrConstant]
  private val pairCache = new mutable.HashMap[(IrConstant, IrConstant), IrConstant]
  private val vectorCache = new mutable.HashMap[Vector[IrConstant], IrConstant]

  // Maximum value of a 32bit unsigned integer
  private val sharedConstantRefCount = (math.pow(2, 32) - 1).toLong

  private def arrayElementsForIrType(irType : IrType) : Int= irType match {
    case ArrayType(elements, _) =>
      elements

    case _ =>
      throw new InternalCompilerErrorException("Expected cell member to be array")
  }

  def defineConstantData(module : IrModuleBuilder)(name : String, initializer : IrConstant) : GlobalVariable = {
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
    val innerConstantName = baseName + ".strByteArray"
    val innerConstantInitializer = StructureConstant(List(
      IntegerConstant(IntegerType(32), sharedConstantRefCount),
      StringConstant(utf8Data)
    ))

    val innerConstant = defineConstantData(module)(innerConstantName, innerConstantInitializer)
    BitcastToConstant(innerConstant, PointerType(UserDefinedType("sharedByteArray")))
  }
  
  private def genInlineUtf8Constant(module : IrModuleBuilder)(utf8Data : Array[Byte], constantLength : Int) : IrConstant = {
    val padByteCount = constantLength - utf8Data.length

    // Pad this string with 0xff
    // This helps test that nobody is relying on these being NULL terminated
    val terminatedUtf8Data = utf8Data ++ Array.fill(padByteCount)(255.toByte)

    // Return a direct constant
    StringConstant(terminatedUtf8Data)
  }

  def genBytevectorCell(module : IrModuleBuilder)(elements : Seq[Short]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementIrs = elements.map(IntegerConstant(IntegerType(8), _))

    val elementsName = baseName + ".elementsByteArray"
    val elementsInitializer = StructureConstant(List(
      IntegerConstant(IntegerType(32), sharedConstantRefCount), 
      ArrayConstant(IntegerType(8), elementIrs)
    ))
    
    val elementsValue = defineConstantData(module)(elementsName, elementsInitializer)

    val bytevectorCellName = baseName + ".cell"
    val bytevectorCell = ct.BytevectorCell.createConstant(
      length=elements.length,
      byteArray=BitcastToConstant(elementsValue, PointerType(UserDefinedType("sharedByteArray")))
    )

    defineConstantData(module)(bytevectorCellName, bytevectorCell)
  }
  
  def genVectorCell(module : IrModuleBuilder)(irElements : Seq[IrConstant]) : IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeVector")

    val elementsName = baseName + ".elements"
    val elementsInitializer = ArrayConstant(PointerType(ct.AnyCell.irType), irElements.toList)
    
    val elementsDef = defineConstantData(module)(elementsName, elementsInitializer)

    val vectorCellName = baseName + ".cell"

    val vectorCell = ct.VectorCell.createConstant(
      length=irElements.length,
      elements=ElementPointerConstant(PointerType(ct.AnyCell.irType), elementsDef, List(0, 0)))

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
      recordData=NullPointerConstant(ct.ProcedureCell.recordDataIrType),
      dataIsInline=1,
      isUndefined=0
    )

    defineConstantData(module)(procCellName, procCell)
  }

  def genStringCell(module : IrModuleBuilder)(value : String) : IrConstant = {
    val baseName = module.nameSource.allocate("schemeString")
    val stringCellName = baseName + ".cell"

    val utf8Data = Codec.toUTF8(value)
    val inlineUtf8Bytes = arrayElementsForIrType(ct.InlineStringCell.inlineDataIrType)

    val stringCell = if (utf8Data.length <= inlineUtf8Bytes) {
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
        heapByteArray=utf8Constant,
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

    val symbolCell = if (utf8Data.length <= inlineUtf8Bytes) {
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
        heapByteArray=utf8Constant,
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

  def apply(state : GenerationState)(createStep : ps.CreateConstant) : IrConstant = {
    val module = state.currentBlock.function.module

    createStep match {
      case ps.CreateStringCell(_, value) =>
        stringCache.getOrElseUpdate(value, {
          genStringCell(module)(value)
        })

      case ps.CreateSymbolCell(_, value) =>
        symbolCache.getOrElseUpdate(value, {
          genSymbolCell(module)(value)
        })

      case ps.CreateExactIntegerCell(_, value) =>
        exactIntegerCache.getOrElseUpdate(value, {
          val intCellName = module.nameSource.allocate("schemeExactInteger")

          val intCell = ct.ExactIntegerCell.createConstant(
            value=value
          )

          defineConstantData(module)(intCellName, intCell)
        })
      
      case ps.CreateFlonumCell(_, value) if value.isNaN =>
        if (!nanCache.isDefined) {
          val flonumCell = ct.FlonumCell.createConstant(
            value=DoubleConstant(value)
          )

          nanCache = Some(
            defineConstantData(module)("schemeNaN", flonumCell)
          )
        }

        nanCache.get
      
      case ps.CreateFlonumCell(_, value) =>
        flonumCache.getOrElseUpdate(value, {
          val flonumCellName = module.nameSource.allocate("schemeFlonum")

          val flonumCell = ct.FlonumCell.createConstant(
            value=DoubleConstant(value)
          )

          defineConstantData(module)(flonumCellName, flonumCell)
        })

      case ps.CreateBooleanCell(_, true) =>
        GlobalDefines.trueIrValue

      case ps.CreateBooleanCell(_, false) =>
        GlobalDefines.falseIrValue
      
      case ps.CreateCharCell(_, value) =>
        characterCache.getOrElseUpdate(value, {
          val charCellName = module.nameSource.allocate("schemeChar")

          val charCell = ct.CharCell.createConstant(
            unicodeChar=value
          )

          defineConstantData(module)(charCellName, charCell)
        })
      
      case ps.CreateBytevectorCell(_, elements) =>
        bytevectorCache.getOrElseUpdate(elements, {
          genBytevectorCell(module)(elements)
        })
      
      case ps.CreateVectorCell(_, elementTemps) =>
        val elementIrs = elementTemps.map { elementTemp =>
          state.liveTemps(elementTemp) match {
            case constant : IrConstant => constant
            case other =>
              throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant car: ${other}")
          }
        }

        vectorCache.getOrElseUpdate(elementIrs, {
          genVectorCell(module)(elementIrs)
        })

      case ps.CreatePairCell(_, carTemp, cdrTemp, listLengthOpt) =>
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

        pairCache.getOrElseUpdate((carIrConstant, cdrIrConstant), {
          val pairCellName = module.nameSource.allocate("schemePair")

          // It's not possible for pairs to have a length of zero - only EmptyLists are zero length
          val listLength = listLengthOpt.getOrElse(0L)

          val pairCell = ct.PairCell.createConstant(
            listLength=listLength,
            car=carIrConstant,
            cdr=cdrIrConstant
          )

          defineConstantData(module)(pairCellName, pairCell)
        })

      case ps.CreateUnitCell(_) =>
        GlobalDefines.unitIrValue

      case ps.CreateEmptyListCell(_) =>
        GlobalDefines.emptyListIrValue

      case ps.CreateEmptyClosure(_, entryPointTemp) =>
        val entryPointConstant = state.liveTemps(entryPointTemp) match {
          case constant : IrConstant => constant
          case other =>
            throw new InternalCompilerErrorException(s"Attempted to create constant closure with non-constant entry point: ${other}")
        }

        genEmptyClosure(module, typeGenerator)(entryPointConstant)

      case ps.CreateNativeInteger(_, value, bits) =>
        IntegerConstant(IntegerType(bits), value)
      
      case ps.CreateNativeFloat(_, value, fpType) if fpType == vt.Double =>
        DoubleConstant(value)

      case ps.CreateNativeFloat(_, value, fpType) if fpType == vt.Float =>
        FloatConstant(value.toFloat)
    }
  }
}

