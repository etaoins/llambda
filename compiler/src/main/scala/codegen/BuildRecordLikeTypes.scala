package io.llambda.compiler.codegen
import io.llambda

import collection.breakOut

import llambda.compiler.planner
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.platform.TargetPlatform
import llambda.llvmir._

object TypeDataStorage extends Enumeration {
  type TypeDataStorage = Value
  val Empty, Inline, OutOfLine = Value
}

/** Generated record-like type
  *
  * @param  recordLikeType     Frontend record type object
  * @param  irType             LLVM IR structure type for the record data
  * @param  classId            Record class ID. This is used for type checks and indexing in to the offset map
  * @param  lastChildClassId   Last class ID of any descendant type. Descendant classes are guaranteed to be assigned
  *                            a contiguous block of class IDs to facilitate simple type checking.
  * @param  fieldToTbaaNode    Mapping of record fields to TBAA metadata nodes
  * @param  fieldToGepIndices  Mapping of record fields to getelementptr indices
  * @param  storageType        Storage type of the record data
  */
case class GeneratedType(
    recordLikeType: vt.RecordLikeType,
    irType: UserDefinedType,
    classId: Long,
    lastChildClassId: Long,
    fieldToTbaaNode: Map[vt.RecordField, Metadata],
    fieldToGepIndices: Map[vt.RecordField, List[Int]],
    storageType: TypeDataStorage.Value
)

object BuildRecordLikeTypes {
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
    val parentStorage = parentGeneratedTypeOpt.map(_.storageType).getOrElse(TypeDataStorage.Empty)

    // Determine our storage type and layout
    val (storageType, fieldOrder) = if (parentStorage != TypeDataStorage.Empty) {
      // Use the same storage as our parent
      (parentStorage, recordLikeType.fields)
    }
    else if (recordLikeType.fields.isEmpty) {
      (TypeDataStorage.Empty, Nil)
    }
    else if (!childTypes.isEmpty) {
      // If we contain data and we have child classes always store out of line to handle the case where our children
      // might overflow our inline size
      (TypeDataStorage.OutOfLine, recordLikeType.fields)
    }
    else {
      val inlineDataSize = (recordLikeType match {
        case _: vt.RecordType =>
          // Records have two pointer sized fields for inline data storage
          2
        case _: vt.ClosureType =>
          // Procedures have one pointer for inline storage. The second pointer points to the procedure's entry point
          1
      }) * (config.targetPlatform.pointerBits / 8)

      // Try to pack the record fields
      val packedRecord = PackRecordLikeInline(recordLikeType, inlineDataSize, config.targetPlatform)

      // Check if we can inline
      val storage = if (packedRecord.inline) {
        TypeDataStorage.Inline
      }
      else {
        TypeDataStorage.OutOfLine
      }

      (storage, packedRecord.fieldOrder)
    }

    // Define the type
    val recordTypeName = module.nameSource.allocate(recordLikeType.sourceName)

    val parentStructOpt = parentGeneratedTypeOpt.map(_.irType)

    val selfFields = fieldOrder.map { field =>
      ValueTypeToIr(recordLikeType.typeForField(field)).irType
    }

    val irType = StructureType(parentStructOpt.toSeq ++ selfFields)

    // Give the type a name
    val userDefinedType = module.nameType(recordTypeName, irType)

    // Make TBAA nodes for each field
    val selfTbaaNodes = (fieldOrder map { field =>
      // Each field cannot be aliased with any other fields
      // If we allow toll-free briding with C structs we'll need to loosen this at least for them
      val nodeName = s"${recordTypeName}::${field.name}"
      val numberedTbaaMetadata = module.numberMetadataNode(
        TbaaMetadata(nodeName)
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

    val initialGeneratedType = GeneratedType(
      recordLikeType=recordLikeType,
      irType=userDefinedType,
      classId=classId,
      lastChildClassId=classId,
      fieldToTbaaNode=fieldToTbaaNode,
      fieldToGepIndices=fieldToGepIndices,
      storageType=storageType
    )

    val childrenResult = childTypes.foldLeft(IntermediateResult(incrementedClassId)) {
      case (IntermediateResult(nextClassId, prevGeneratedTypes), childType) =>
        val childResult = generateType(nextClassId, Some(initialGeneratedType), childType)

        IntermediateResult(
          childResult.nextClassId,
          prevGeneratedTypes ++ childResult.generatedTypes
        )
    }

    // Add our last child class ID for type checks
    val finalGeneratedType = initialGeneratedType.copy(
      lastChildClassId=childrenResult.nextClassId - 1
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
      val recordSize = GenSizeOf(generatedType.irType, offsetIrType)

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
