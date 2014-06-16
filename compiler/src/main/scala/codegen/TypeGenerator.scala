package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.llvmir._
import llambda.compiler.platform.TargetPlatform
import llambda.compiler.InternalCompilerErrorException

object TypeDataStorage extends Enumeration {
  type TypeDataStorage = Value
  val Empty, Inline, OutOfLine = Value
}

case class GeneratedType(
  recordLikeType : vt.RecordLikeType,
  irType : UserDefinedType,
  classId : Long,
  fieldToStructIndex : Map[vt.RecordField, Int],
  fieldToTbaaNode : Map[vt.RecordField, Metadata],
  storageType : TypeDataStorage.Value
)

class TypeGenerator(module : IrModuleBuilder, targetPlatform : TargetPlatform) {
  private val generatedTypes = collection.mutable.Map[vt.RecordLikeType, GeneratedType]()
  private var nextClassId : Long = 0

  // Ensure empty closures always have class ID 0
  // This is to implement eqv? properly for closureless procedures boxed in different locations 
  apply(vt.EmptyClosureType)

  def apply(recordLikeType : vt.RecordLikeType) : GeneratedType = {
    generatedTypes.getOrElseUpdate(recordLikeType, {
      // Determine our storage type and layout
      val (storageType, fieldOrder) = if (recordLikeType.fields.isEmpty) {
        (TypeDataStorage.Empty, Nil)
      }
      else {
        val inlineDataSize = (recordLikeType match {
          case _ : vt.RecordType =>
            // Records have two pointer sized fields for inline data storage
            2
          case _ : vt.ClosureType =>
            // Procedures have one pointer for inline storage. The second pointer points to the procedure's entry point
            1
        }) * (targetPlatform.pointerBits / 8) 

        // Try to pack the record fields
        val packedRecord = PackRecordLikeInline(recordLikeType.fields, inlineDataSize, targetPlatform)

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

      val irType = StructureType(fieldOrder.map { field =>
        ValueTypeToIr(field.fieldType).irType
      })

      // Give the type a name
      val userDefinedType = module.nameType(recordTypeName, irType)

      // Make TBAA nodes for each field
      val fieldToTbaaNode = (recordLikeType.fields map { field =>
        // Each field cannot be aliased with any other fields
        // If we allow toll-free briding with C structs we'll need to loosen this at least for them
        val nodeName = s"${recordTypeName}::${field.sourceName}"
        val numberedTbaaMetadata = module.numberMetadataNode(
          TbaaMetadata(nodeName)
        )

        (field, numberedTbaaMetadata)
      }).toMap

      // Allocate it a class ID
      val classId = nextClassId
      nextClassId = nextClassId + 1

      GeneratedType(
        recordLikeType=recordLikeType,
        irType=userDefinedType,
        classId=classId,
        fieldToStructIndex=fieldOrder.zipWithIndex.toMap,
        fieldToTbaaNode=fieldToTbaaNode,
        storageType=storageType)
    })
  }
  
  def emitTypeMaps() {
    val offsetIrType = IntegerType(32)

    // Output the type maps for each generated type
    val typeMapVars = generatedTypes.values.toList.sortBy(_.classId).map { generatedType =>
      val recordLikeType = generatedType.recordLikeType
      val recordCellNullPointer = NullPointerConstant(PointerType(generatedType.irType))

      // Generate an expression for the offset of each field that points to a cell
      val cellOffsets = recordLikeType.fields.zipWithIndex.filter({ case (field, _) =>
        // We only want GC managed types
        field.fieldType.isGcManaged
      }).map({ case (field, fieldIndex) =>
        val fieldIrType = ValueTypeToIr(field.fieldType).irType

        // Get a field pointer based on a null record cell pointer
        // This will generate a byte offset
        val fieldPointer = ElementPointerConstant(fieldIrType, recordCellNullPointer, List(0, fieldIndex))

        // Convert it to i32
        PtrToIntConstant(fieldPointer, offsetIrType)
      })

      if (cellOffsets.isEmpty) {
        // Nothing to do!
        None
      }
      else {
        val headerValue = IntegerConstant(offsetIrType, generatedType.storageType match {
          case TypeDataStorage.Empty =>
            throw new InternalCompilerErrorException("Attempted to generate type map for type with no data")              

          case _ =>
            cellOffsets.length 
        })

        // Make it in to an IR constant
        val cellOffsetsConstant = ArrayConstant(offsetIrType, headerValue :: cellOffsets) 

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
        Some(typeMapDef.variable)
      }
    } : List[Option[GlobalVariable]]

    // Now output the class map
    val classMapEntries = typeMapVars.map { variableOpt =>
      variableOpt match {
        case None =>
          // There's no type map
          NullPointerConstant(PointerType(offsetIrType))

        case Some(variable) =>
          // Convert the array type to a pointer type
          ElementPointerConstant(offsetIrType, variable, List(0, 0))
      }
    }

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
}
