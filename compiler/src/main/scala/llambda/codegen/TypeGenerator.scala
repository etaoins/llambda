package llambda.codegen

import llambda.{valuetype => vt}
import llambda.codegen.llvmir._
import llambda.platform.TargetPlatform
import llambda.InternalCompilerErrorException

object TypeDataStorage extends Enumeration {
  type TypeDataStorage = Value
  val Empty, Inline, OutOfLine = Value
}

case class GeneratedType(
  recordType : vt.RecordCellType,
  irType : UserDefinedType,
  classId : Long,
  fieldToStructIndex : Map[vt.RecordField, Int],
  fieldToTbaaIndex : Map[vt.RecordField, Long],
  storageType : TypeDataStorage.Value
)

class TypeGenerator(module : IrModuleBuilder, targetPlatform : TargetPlatform, var nextTbaaIndex : Long) {
  private val generatedTypes = collection.mutable.Map[vt.RecordCellType, GeneratedType]()
  private var nextClassId : Long = 0

  def apply(recordType : vt.RecordCellType) : GeneratedType = {
    generatedTypes.getOrElseUpdate(recordType, {
      // Determine our storage type and layout
      val (storageType, fieldOrder) = if (recordType.fields.isEmpty) {
        (TypeDataStorage.Empty, Nil)
      }
      else {
        // Records have two pointer sized fields for inline data storage
        val inlineDataSize = (targetPlatform.pointerBits * 2) / 8

        // Try to pack the record fields
        val packedRecord = PackRecordInline(recordType.fields, inlineDataSize, targetPlatform)

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
      val recordTypeName = module.nameSource.allocate(recordType.sourceName)

      val irType = StructureType(fieldOrder.map { field =>
        ValueTypeToIr(field.fieldType).irType
      })

      // Give the type a name
      val userDefinedType = module.nameType(recordTypeName, irType)

      // Make TBAA nodes for each field
      val fieldToTbaaIndex = (recordType.fields map { field =>
        val tbaaIndex = nextTbaaIndex
        nextTbaaIndex = nextTbaaIndex + 1

        // Each field cannot be aliased with any other fields
        // If we allow toll-free briding with C structs we'll need to loosen
        // this at least for them
        val nodeName = s"${recordType.sourceName}::${field.sourceName}"
        module.defineTbaaNode(IrTbaaNode(tbaaIndex, nodeName))

        (field, tbaaIndex)
      }).toMap

      // Allocate it a class ID
      val classId = nextClassId
      nextClassId = nextClassId + 1

      GeneratedType(
        recordType=recordType,
        irType=userDefinedType,
        classId=classId,
        fieldToStructIndex=fieldOrder.zipWithIndex.toMap,
        fieldToTbaaIndex=fieldToTbaaIndex,
        storageType=storageType)
    })
  }
  
  def emitTypeMaps() {
    val offsetIrType = IntegerType(32)

    // Output the type maps for each generated type
    val typeMapVars = generatedTypes.values.toList.sortBy(_.classId).map { generatedType =>
      val recordCellType = generatedType.recordType
      val recordCellNullPointer = NullPointerConstant(PointerType(generatedType.irType))

      // Generate an expression for the offset of each field that points to a
      // cell
      val cellOffsets = recordCellType.fields.zipWithIndex.filter({ case (field, _) =>
        // We only want value cell types
        field.fieldType.isInstanceOf[vt.CellValueType]
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

          case TypeDataStorage.Inline =>
            // Flag this data as inline 
            cellOffsets.length | 0x800000
            
          case TypeDataStorage.OutOfLine =>
            // No flag indicates this is out-of-line
            cellOffsets.length 
        })

        // Make it in to an IR constant
        val cellOffsetsConstant = ArrayConstant(offsetIrType, headerValue :: cellOffsets) 

        // Define it
        val typeMapName = module.nameSource.allocate(recordCellType.sourceName + "Map")

        // XXX: Clang 3.4 doesn't actually merge these constants. We should
        // investigate doing merging of obviously identical maps ourselves
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
      name="_llambda_class_map",
      initializer=classMapConstant,
      linkage=Linkage.External,
      unnamedAddr=true,
      constant=true
    )

    module.defineGlobalVariable(classMapDef)
  }
}
