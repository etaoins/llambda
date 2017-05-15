package io.llambda.compiler.codegen
import io.llambda

import scala.io.Codec

import scala.collection.mutable

import llambda.compiler.InternalCompilerErrorException

import llambda.compiler.planner.{step => ps}
import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

class ConstantGenerator(generatedTypes: Map[vt.RecordLikeType, GeneratedType]) {
  /* Caches of constants indexed by their value
   *
   * This ensures proper Scheme semantics are maintained with the planner's lazy value instantiation. For example,
   * the following code:
   * (define x '(1 2 3))
   * (eqv? x x)
   *
   * Will actually plan the list for 'x' once for each argument. This breaks Scheme semantics as (eqv?) would return
   * false if a separate instance was returned for each argument.
   */

  private val stringCache = new mutable.HashMap[String, IrConstant]
  private val symbolCache = new mutable.HashMap[String, IrConstant]
  private val integerCache = new mutable.LongMap[IrConstant]
  private val flonumCache = new mutable.LongMap[IrConstant]
  private val characterCache = new mutable.LongMap[IrConstant]
  private val bytevectorCache = new mutable.HashMap[Vector[Short], IrConstant]
  private val pairCache = new mutable.HashMap[(IrConstant, IrConstant), IrConstant]
  private val vectorCache = new mutable.HashMap[Vector[IrConstant], IrConstant]
  private val recordCache = new mutable.HashMap[(vt.RecordType, Map[vt.RecordField, IrConstant], Boolean), IrConstant]

  // Maximum value of a 32bit unsigned integer
  private val sharedConstantRefCount = (math.pow(2, 32) - 1).toLong

  def defineConstantData(module: IrModuleBuilder)(name: String, initializer: IrConstant): GlobalVariable = {
    val constantDataDef = IrGlobalVariableDef(
      name=name,
      initializer=initializer,
      linkage=Linkage.Private,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(constantDataDef)

    constantDataDef.variable
  }

  private val uninitialisedHashValue = 0
  private val uninitialisedHashRemapValue = 0x86b2bb0d

  private val FNV1APrime = 0x1000193
  private val FNV1AOffsetBasis = -0x7EE3623B // 0x811C9DC5 as signed 32bit

  private def sharedShortHash(bytes: Seq[Short]): Int = {
    val hash = bytes.foldLeft(FNV1AOffsetBasis) { (hash, byte) => (hash ^ (byte & 0xff)) * FNV1APrime }
    if (hash == uninitialisedHashValue) uninitialisedHashRemapValue else hash
  }

  private def sharedByteHash(bytes: Seq[Byte]): Int = {
    val hash = bytes.foldLeft(FNV1AOffsetBasis) { (hash, byte) => (hash ^ (byte & 0xff)) * FNV1APrime }
    if (hash == uninitialisedHashValue) uninitialisedHashRemapValue else hash
  }

  private def genHeapUtf8Constant(module: IrModuleBuilder)(baseName: String, utf8Data: Array[Byte]): IrConstant = {
    val innerConstantName = baseName + ".strByteArray"
    val innerConstantInitializer = StructureConstant(List(
      IntegerConstant(IntegerType(32), sharedConstantRefCount),
      IntegerConstant(IntegerType(32), sharedByteHash(utf8Data)),
      StringConstant(utf8Data)
    ))

    val innerConstant = defineConstantData(module)(innerConstantName, innerConstantInitializer)
    BitcastToConstant(innerConstant, PointerType(UserDefinedType("sharedByteArray")))
  }

  private def genInlineUtf8Constant(module: IrModuleBuilder)(utf8Data: Array[Byte], constantLength: Int): IrConstant = {
    val padByteCount = constantLength - utf8Data.length

    // Pad this string with 0xff
    // This helps test that nobody is relying on these being NULL terminated
    val terminatedUtf8Data = utf8Data ++ Array.fill(padByteCount)(255.toByte)

    // Return a direct constant
    StringConstant(terminatedUtf8Data)
  }

  private def charLengthForString(stringValue: String): Int = {
    // Java is internally based on UTF-16. For Unicode code points outside the BMP it uses surrogate pairs which count
    // as two Java "characters"
    // Llambda is native UTF-8 and considers a code point the same as a character
    stringValue.codePointCount(0, stringValue.length)
  }

  private def genBytevectorCell(module: IrModuleBuilder)(elements: Seq[Short]): IrConstant = {
    // Make our elements
    val baseName = module.nameSource.allocate("schemeBytevector")

    val elementIrs = elements.map(IntegerConstant(IntegerType(8), _))

    val elementsName = baseName + ".elementsByteArray"
    val elementsInitializer = StructureConstant(List(
      IntegerConstant(IntegerType(32), sharedConstantRefCount),
      IntegerConstant(IntegerType(32), sharedShortHash(elements)),
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

  private def genVectorCell(module: IrModuleBuilder)(irElements: Seq[IrConstant]): IrConstant = {
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

  private def genRecordLikeData(module: IrModuleBuilder, genGlobals: GenGlobals)(
      recordLikeType: vt.RecordLikeType,
      fieldTemps: Map[vt.RecordField, IrConstant]
  ): StructureConstant = {
    val generatedType = genGlobals.generatedTypes(recordLikeType)

    // Generate our parent type
    val parentDataOpt = recordLikeType.parentRecordOpt map { parentType =>
      genRecordLikeData(module, genGlobals)(parentType, fieldTemps)
    }

    // Find our self fields and order them
    val orderedSelfFields = generatedType.fieldToGepIndices.toList.filter({ case (field, gepIndices) =>
      // Our self fields will only have a single GEP index
      gepIndices.length == 1
    }).sortBy(_._2(0))

    val fieldData = orderedSelfFields.map { case (field, _) =>
      fieldTemps(field)
    }

    // Build the structure
    StructureConstant(parentDataOpt.toList ++ fieldData, userDefinedType=Some(generatedType.irType))
  }

  private def genRecordCell(module: IrModuleBuilder, genGlobals: GenGlobals)(
      recordType: vt.RecordType,
      fieldTemps: Map[vt.RecordField, IrConstant],
      isUndefined: Boolean
  ): IrConstant = {
    val generatedType = genGlobals.generatedTypes(recordType)

    val baseName = module.nameSource.allocate("schemeRecord")
    val recordCellName = baseName + ".cell"

    generatedType.storageType match {
      case TypeDataStorage.Empty =>
        val recordDataNullPtrIr = NullPointerConstant(ct.RecordCell.recordDataIrType)
        val extraDataNullIr = ArrayConstant(IntegerType(8),
          List.fill(ct.RecordCell.extraDataIrType.elements)(IntegerConstant(IntegerType(8), 0))
        )

        val recordCell = ct.RecordCell.createConstant(
          extraData=extraDataNullIr,
          dataIsInline=1,
          isUndefined=if (isUndefined) 1 else 0,
          recordClassId=generatedType.classId,
          recordData=recordDataNullPtrIr
        )

        defineConstantData(module)(recordCellName, recordCell)

      case TypeDataStorage.Inline =>
        val recordData = genRecordLikeData(module, genGlobals)(recordType, fieldTemps)

        // This is a bit of a hack. Record cells are defined to have a data pointer which we need to replace with our
        // inline data. Open code the record structure and append our data to the end
        val inlineRecordCell = StructureConstant(List(
          ct.AnyCell.createConstant(typeId=ct.RecordCell.typeId),
          IntegerConstant(ct.RecordCell.dataIsInlineIrType, 1),
          IntegerConstant(ct.RecordCell.isUndefinedIrType, if (isUndefined) 1 else 0),
          IntegerConstant(ct.RecordCell.recordClassIdIrType, generatedType.classId)
        ))

        val packedCell = StructureConstant(List(inlineRecordCell, recordData))
        val packedCellDef = defineConstantData(module)(recordCellName, packedCell)

        BitcastToConstant(packedCellDef, PointerType(ct.RecordCell.irType))

      case TypeDataStorage.OutOfLine =>
        val extraDataNullIr = ArrayConstant(IntegerType(8),
          List.fill(ct.RecordCell.extraDataIrType.elements)(IntegerConstant(IntegerType(8), 0))
        )

        val recordDataName = baseName + ".data"

        val recordData = genRecordLikeData(module, genGlobals)(recordType, fieldTemps)
        val recordDataDef = defineConstantData(module)(recordDataName, recordData)

        val recordCell = ct.RecordCell.createConstant(
          extraData=extraDataNullIr,
          dataIsInline=0,
          isUndefined=if (isUndefined) 1 else 0,
          recordClassId=generatedType.classId,
          recordData=BitcastToConstant(recordDataDef, ct.RecordCell.recordDataIrType)
        )

        defineConstantData(module)(recordCellName, recordCell)
    }
  }

  private def genEmptyClosure(module: IrModuleBuilder)(entryPoint: IrConstant): IrConstant = {
    val procCellName = module.nameSource.allocate("schemeProcedure")

    // Find the class ID for the empty closure type
    val generatedType = generatedTypes(vt.EmptyClosureType)

    val extraDataNullIr = ArrayConstant(IntegerType(8),
      List.fill(ct.ProcedureCell.extraDataIrType.elements)(IntegerConstant(IntegerType(8), 0))
    )

    val procCell = ct.ProcedureCell.createConstant(
      extraData=extraDataNullIr,
      entryPoint=entryPoint,
      recordClassId=generatedType.classId,
      // This can be anything as it's unused. Dynamic record cells leave this uninitialized to be instruction thrifty
      // but we have to supply a value here
      recordData=NullPointerConstant(ct.ProcedureCell.recordDataIrType),
      dataIsInline=1,
      isUndefined=0
    )

    defineConstantData(module)(procCellName, procCell)
  }

  private def genStringCell(module: IrModuleBuilder)(value: String): IrConstant = {
    val baseName = module.nameSource.allocate("schemeString")
    val stringCellName = baseName + ".cell"

    val utf8Data = Codec.toUTF8(value)
    val inlineUtf8Bytes = ct.StringCellConstants.maxInlineStringBytes

    val stringCell = if (utf8Data.length <= inlineUtf8Bytes) {
      // We can do this inline
      val utf8Constant = genInlineUtf8Constant(module)(utf8Data, inlineUtf8Bytes)

      ct.InlineStringCell.createConstant(
        inlineCharLength=charLengthForString(value),
        inlineData=utf8Constant,
        inlineByteLength=utf8Data.length
      )
    }
    else {
      // We have to put this on the heap
      val utf8Constant = genHeapUtf8Constant(module)(baseName, utf8Data)

      ct.HeapStringCell.createConstant(
        heapByteLength=utf8Data.length,
        heapCharLength=charLengthForString(value),
        heapByteArray=utf8Constant,
        inlineByteLength=ct.SymbolCellConstants.heapSymbolInlineByteLength
      )
    }

    // The rest of the compiler assumes this is just a %string*
    BitcastToConstant(
      defineConstantData(module)(stringCellName, stringCell),
      PointerType(ct.StringCell.irType)
    )
  }

  private def genSymbolCell(module: IrModuleBuilder)(value: String): IrConstant = {
    val baseName = module.nameSource.allocate("schemeSymbol")
    val symbolCellName = baseName + ".cell"

    val utf8Data = Codec.toUTF8(value)
    val inlineUtf8Bytes = ct.SymbolCellConstants.maxInlineSymbolBytes

    val symbolCell = if (utf8Data.length <= inlineUtf8Bytes) {
      // We can do this inline
      val utf8Constant = genInlineUtf8Constant(module)(utf8Data, inlineUtf8Bytes)

      ct.InlineSymbolCell.createConstant(
        inlineCharLength=charLengthForString(value),
        inlineData=utf8Constant,
        inlineByteLength=utf8Data.length
      )
    }
    else {
      // We have to put this on the heap
      val utf8Constant = genHeapUtf8Constant(module)(baseName, utf8Data)

      ct.HeapSymbolCell.createConstant(
        heapByteLength=utf8Data.length,
        heapCharLength=charLengthForString(value),
        heapByteArray=utf8Constant,
        inlineByteLength=ct.SymbolCellConstants.heapSymbolInlineByteLength
      )
    }

    // The rest of the compiler assumes this is just a %symbol*
    BitcastToConstant(
      defineConstantData(module)(symbolCellName, symbolCell),
      PointerType(ct.SymbolCell.irType)
    )
  }

  def apply(state: GenerationState, genGlobals: GenGlobals)(createStep: ps.CreateConstant): IrConstant = {
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

      case ps.CreateIntegerCell(_, value) =>
        integerCache.getOrElseUpdate(value, {
          val intCellName = module.nameSource.allocate("schemeInteger")

          val intCell = ct.IntegerCell.createConstant(
            value=value
          )

          defineConstantData(module)(intCellName, intCell)
        })

      case ps.CreateFlonumCell(_, value) =>
        // Use longBits here so we distinguish +nan.0, -0.0, etc properly
        val longBits = java.lang.Double.doubleToLongBits(value)

        flonumCache.getOrElseUpdate(longBits, {
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
            case constant: IrConstant => constant
            case other =>
              throw new InternalCompilerErrorException(s"Attempted to create constant vector with non-constant element: ${other}")
          }
        }

        vectorCache.getOrElseUpdate(elementIrs, {
          genVectorCell(module)(elementIrs)
        })

      case ps.CreateRecordCell(_, recordType, fieldTemps, isUndefined) =>
        val fieldIrs = fieldTemps.map { case (field, fieldTemp) =>
          state.liveTemps(fieldTemp) match {
            case constant: IrConstant =>
              field -> constant

            case other =>
              throw new InternalCompilerErrorException(s"Attempted to create constant record with non-constant field: ${other}")
          }
        }

        val cacheKey = (recordType, fieldIrs, isUndefined)

        recordCache.getOrElseUpdate(cacheKey, {
          genRecordCell(module, genGlobals)(recordType, fieldIrs, isUndefined)
        })

      case ps.CreatePairCell(_, carTemp, cdrTemp, listLengthOpt) =>
        val carIrConstant = state.liveTemps(carTemp) match {
          case constant: IrConstant => constant
          case other =>
            throw new InternalCompilerErrorException(s"Attempted to create constant pair with non-constant car: ${other}")
        }

        val cdrIrConstant = state.liveTemps(cdrTemp) match {
          case constant: IrConstant => constant
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
          case constant: IrConstant => constant
          case other =>
            throw new InternalCompilerErrorException(s"Attempted to create constant closure with non-constant entry point: ${other}")
        }

        // Cast to an untyped entry point
        val castEntryPoint = BitcastToConstant(entryPointConstant, ct.ProcedureCell.entryPointIrType)
        genEmptyClosure(module)(castEntryPoint)

      case ps.CreateNativeInteger(_, value, bits) =>
        IntegerConstant(IntegerType(bits), value)

      case ps.CreateNativeFloat(_, value, fpType) if fpType == vt.Double =>
        DoubleConstant(value)

      case ps.CreateNativeFloat(_, value, fpType) if fpType == vt.Float =>
        FloatConstant(value.toFloat)
    }
  }
}
