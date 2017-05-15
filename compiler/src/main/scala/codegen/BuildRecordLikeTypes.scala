package io.llambda.compiler.codegen
import io.llambda

import collection.breakOut

import llambda.compiler.planner
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.platform.TargetPlatform
import llambda.llvmir._

object TypeDataStorage extends Enumeration {
  type TypeDataStorage = Value
  val Empty, Inline, OutOfLine = Value
}

/** Generated record-like type
  *
  * @param  recordLikeType           Frontend record type object
  * @param  irType                   LLVM IR user defined type for the record data
  * @param  expandedStructType       Structure type for the record data including the structure type for all parents.
  *                                  This is used to calculate the size of descendant record data structs.
  * @param  classId                  Record class ID. This is used for type checks and indexing in to the offset map
  * @param  lastChildClassId         Last class ID of any descendant type. Descendant classes are guaranteed to be
  *                                  assigned a contiguous block of class IDs to facilitate simple type checking.
  * @param  fieldToTbaaNode          Mapping of record fields to TBAA metadata nodes
  * @param  fieldToGepIndices        Mapping of record fields to getelementptr indices
  * @param  storageType              Storage type of the record data
  * @param  hasOutOfLineDescendants  Indicates if any descendant classes have out-of-line data storage. This is used to
  *                                  determine if dataIsInline needs to be consulted when loading record-like data.
  * @param  sizeBytes                Size of the record data in bytes.
  */
case class GeneratedType(
    recordLikeType: vt.RecordLikeType,
    irType: UserDefinedType,
    expandedStructType: StructureType,
    classId: Long,
    lastChildClassId: Long,
    fieldToTbaaNode: Map[vt.RecordField, Metadata],
    fieldToGepIndices: Map[vt.RecordField, List[Int]],
    storageType: TypeDataStorage.Value,
    hasOutOfLineDescendants: Boolean,
    sizeBytes: Long
)

private[codegen] object BuildRecordLikeTypes {
  private val rootTbaaNode = NumberedMetadata(0)

  private case class BuildConfig(
      module: IrModuleBuilder,
      childTypesByParent: Map[vt.RecordLikeType, List[vt.RecordLikeType]],
      targetPlatform: TargetPlatform
  )

  private case class IntermediateResult(
      nextClassId: Long = 0,
      generatedTypes: Map[vt.RecordLikeType, GeneratedType] = Map()
  )

  private def typeAndAllParents(recordLikeType: vt.RecordLikeType): List[vt.RecordLikeType] = {
    recordLikeType :: recordLikeType.parentRecordOpt.map(typeAndAllParents).getOrElse(Nil)
  }

  private def findAllRecordTypes(steps: Iterable[ps.Step]): List[vt.RecordLikeType] = {
    (steps.flatMap {
      case recordStep: ps.RecordLikeStep =>
        typeAndAllParents(recordStep.recordLikeType)

      case condBranch: ps.CondBranch =>
        condBranch.innerBranches.flatMap({ case (steps, _) =>
          findAllRecordTypes(steps).toSeq
        })

      case _ =>
        Set[vt.RecordLikeType]()
    })(breakOut)
  }

  private def generateType(
      nextClassId: Long,
      parentGeneratedTypeOpt: Option[GeneratedType],
      recordLikeType: vt.RecordLikeType
  )(implicit config: BuildConfig): IntermediateResult = {
    val module = config.module

    val childTypes = config.childTypesByParent.get(recordLikeType).getOrElse(Nil)

    // Calculate the size of our inline data area
    val inlineDataStruct = StructureType(recordLikeType match {
      case _: vt.RecordType =>
        List(ct.RecordLikeCell.recordDataIrType, ct.RecordCell.extraDataIrType)
      case _: vt.ClosureType =>
        List(ct.RecordLikeCell.recordDataIrType, ct.ProcedureCell.extraDataIrType)
    })

    val inlineDataBytes = LayoutForIrType(config.targetPlatform.dataLayout)(inlineDataStruct).sizeBits / 8

    // Try to pack the record fields
    val parentExpandedStructTypeOpt = parentGeneratedTypeOpt.map(_.expandedStructType)
    val packedRecord = PackRecordLike(parentExpandedStructTypeOpt, recordLikeType, config.targetPlatform)
    val fieldOrder = packedRecord.fieldOrder

    // Check if we can inline
    val storageType = if (packedRecord.sizeBytes == 0) {
      TypeDataStorage.Empty
    }
    else if (packedRecord.sizeBytes <= inlineDataBytes) {
      TypeDataStorage.Inline
    }
    else {
      TypeDataStorage.OutOfLine
    }

    // Define the type
    val recordTypeName = module.nameSource.allocate(recordLikeType.sourceName)

    val selfFields = fieldOrder.map { field =>
      ValueTypeToIr(recordLikeType.typeForField(field)).irType
    }

    val parentIrTypeOpt = parentGeneratedTypeOpt.map(_.irType)
    val structType = StructureType(parentIrTypeOpt.toSeq ++ selfFields)
    val expandedStructType = StructureType(parentExpandedStructTypeOpt.toSeq ++ selfFields)

    // Give the type a name
    val userDefinedType = module.nameType(recordTypeName, structType)

    // Make TBAA nodes for each field
    val selfTbaaNodes = (fieldOrder map { field =>
      // Each field cannot be aliased with any other fields
      // If we allow toll-free briding with C structs we'll need to loosen this at least for them
      val nodeName = s"${recordTypeName}::${field.name}"
      val numberedTbaaMetadata = module.numberMetadataNode(
        TbaaMetadata(nodeName, Some(rootTbaaNode))
      )

      field -> numberedTbaaMetadata
    }).toSeq

    val inheritedTbaaNodes = parentGeneratedTypeOpt.map { parentGeneratedType =>
      (parentGeneratedType.fieldToTbaaNode.map { case (field, parentNode) =>
        val nodeName = s"${recordTypeName}::${field.name}"
        val numberedTbaaMetadata = module.numberMetadataNode(
          TbaaMetadata(nodeName, Some(parentNode))
        )

        field -> numberedTbaaMetadata
      }).toSeq
    } getOrElse(Nil)

    val fieldToTbaaNode = (selfTbaaNodes ++ inheritedTbaaNodes).toMap

    val selfFieldToGepIndices = fieldOrder.zipWithIndex.map { case (field, index) =>
      // Offset to account for our parent field
      val parentOffset = if (parentGeneratedTypeOpt.isDefined) 1 else 0
      field -> List(index + parentOffset)
    }

    val inheritedFieldtoGepIndices = parentGeneratedTypeOpt.map { parentGeneratedType =>
      (parentGeneratedType.fieldToGepIndices.map { case (field, gepIndices) =>
        field -> (0 :: gepIndices)
      }).toSeq
    } getOrElse(Nil)

    val fieldToGepIndices = (selfFieldToGepIndices ++ inheritedFieldtoGepIndices).toMap

    // Allocate it a class ID
    val classId = nextClassId
    val incrementedClassId = classId + 1

    val initialGeneratedType: GeneratedType = GeneratedType(
      recordLikeType=recordLikeType,
      irType=userDefinedType,
      expandedStructType=expandedStructType,
      classId=classId,
      lastChildClassId=classId,
      fieldToTbaaNode=fieldToTbaaNode,
      fieldToGepIndices=fieldToGepIndices,
      storageType=storageType,
      hasOutOfLineDescendants=false,
      sizeBytes=packedRecord.sizeBytes
    )

    val childrenResult = childTypes.foldLeft(IntermediateResult(incrementedClassId)) {
      case (IntermediateResult(nextClassId, prevGeneratedTypes), childType) =>
        val childResult = generateType(nextClassId, Some(initialGeneratedType), childType)

        IntermediateResult(
          childResult.nextClassId,
          prevGeneratedTypes ++ childResult.generatedTypes
        )
    }

    val hasOutOfLineDescendants = childrenResult.generatedTypes.exists { case (_, childType) =>
      childType.hasOutOfLineDescendants || (childType.storageType == TypeDataStorage.OutOfLine)
    }

    val finalGeneratedType = initialGeneratedType.copy(
      lastChildClassId=childrenResult.nextClassId - 1,
      hasOutOfLineDescendants=hasOutOfLineDescendants
    )

    childrenResult.copy(
      generatedTypes=childrenResult.generatedTypes + (recordLikeType -> finalGeneratedType)
    )
  }

  private def emitTypeMaps(module: IrModuleBuilder, generatedTypes: Map[vt.RecordLikeType, GeneratedType]) {
    val offsetIrType = IntegerType(32)

    // Output the type maps for each generated type
    val classMapEntries = generatedTypes.values.toList.sortBy(_.classId).map { generatedType =>
      val recordLikeType = generatedType.recordLikeType
      val recordCellNullPointer = NullPointerConstant(PointerType(generatedType.irType))

      val fieldToTypeWithInherited = recordLikeType.fieldsWithInherited map { field =>
        field -> recordLikeType.typeForField(field)
      }

      // Generate an expression for the offset of each field that points to a cell
      val cellOffsets = fieldToTypeWithInherited.filter(_._2.isGcManaged) map { case (field, fieldType) =>
        val fieldIndices = generatedType.fieldToGepIndices(field)
        val fieldIrType = ValueTypeToIr(fieldType).irType

        // Get a field pointer based on a null record cell pointer
        // This will generate a byte offset
        val fieldPointer = ElementPointerConstant(fieldIrType, recordCellNullPointer, (0 :: fieldIndices))

        // Convert it to i32
        PtrToIntConstant(fieldPointer, offsetIrType)
      }

      // Total size of the record. This is needed to implement cloning records with native values.
      val recordSize = IntegerConstant(offsetIrType, generatedType.sizeBytes)

      val cellOffsetCount = IntegerConstant(offsetIrType, cellOffsets.length)

      // Make it in to an IR constant
      val cellOffsetsConstant = ArrayConstant(offsetIrType, recordSize :: cellOffsetCount :: cellOffsets)

      // Define it
      val typeMapName = module.nameSource.allocate(recordLikeType.sourceName + "Map")

      // XXX: Clang 3.3 doesn't actually merge these constants. We should investigate doing merging of obviously
      // identical maps ourselves
      val typeMapDef = IrGlobalVariableDef(
        name=typeMapName,
        initializer=cellOffsetsConstant,
        linkage=Linkage.Private,
        unnamedAddr=true,
        constant=true
      )

      module.defineGlobalVariable(typeMapDef)

      ElementPointerConstant(offsetIrType, typeMapDef.variable, List(0, 0))
    }: List[IrConstant]

    val classMapConstant = ArrayConstant(PointerType(offsetIrType), classMapEntries)

    // This needs to be externally visible for our stdlib
    val classMapDef = IrGlobalVariableDef(
      name="_llambda_compiler_class_map",
      initializer=classMapConstant,
      linkage=Linkage.External,
      unnamedAddr=true,
      constant=true
    )

    module.defineGlobalVariable(classMapDef)
  }

  def apply(
      module: IrModuleBuilder,
      functions: Map[String, planner.PlannedFunction],
      targetPlatform: TargetPlatform
  ): Map[vt.RecordLikeType, GeneratedType] = {
    // Make sure EmptyClosureType is always type ID 0 as the runtime uses this to implement (eqv?) for non-capturing
    // procedures
    val allTypes = (vt.EmptyClosureType :: findAllRecordTypes(functions.flatMap(_._2.steps))).distinct

    // Build a inheritance-based type tree
    val groupedTypes = allTypes.groupBy(_.parentRecordOpt)

    val rootTypes = groupedTypes.get(None).getOrElse(Nil)
    val childTypesByParent = groupedTypes collect { case (Some(parent), child) =>
      parent -> child
    }

    // Build our configuration object
    implicit val config = BuildConfig(module, childTypesByParent, targetPlatform)

    // Iterate through our roots to build our types
    val finalResult = rootTypes.foldLeft(IntermediateResult()) {
      case (IntermediateResult(nextClassId, prevGeneratedTypes), rootType) =>
        val rootResult = generateType(nextClassId, None, rootType)

        IntermediateResult(
          rootResult.nextClassId,
          prevGeneratedTypes ++ rootResult.generatedTypes
        )
    }

    emitTypeMaps(module, finalResult.generatedTypes)
    finalResult.generatedTypes
  }
}
