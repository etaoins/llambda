package llambda.codegen

import llambda.{valuetype => vt}
import llambda.codegen.llvmir._

object RecordDataStorage extends Enumeration {
  type RecordDataStorage = Value
  val Empty, Inline, OutOfLine = Value
}

case class GeneratedRecordType(
  recordType : vt.RecordCellType,
  irType : UserDefinedType,
  classId : Long,
  fieldToTbaaIndex : Map[vt.RecordField, Long],
  storageType : RecordDataStorage.Value
)

class RecordTypeGenerator(module : IrModuleBuilder, var nextTbaaIndex : Long) {
  private val generatedRecordTypes = collection.mutable.Map[vt.RecordCellType, GeneratedRecordType]()
  private var nextClassId : Long = 1

  def apply(recordType : vt.RecordCellType) : GeneratedRecordType = {
    generatedRecordTypes.getOrElseUpdate(recordType, {
      val recordTypeName = module.nameSource.allocate(recordType.sourceName)

      val irType = StructureType(recordType.fields.map { field =>
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

      val storageType = recordType.fields match {
        case Nil =>
          // No data to store
          RecordDataStorage.Empty

        case singleField :: Nil =>
          // If there's only a single field we can inline the data
          // XXX: This is broken on 32bit because we're inlining over a pointer
          // and Int64 and Double are larger than 32bits. The alignment could also
          // be incorrect on 32bit.
          RecordDataStorage.Inline

        case _ =>
          RecordDataStorage.OutOfLine
      }

      // Allocate it a class ID
      val classId = nextClassId
      nextClassId = nextClassId + 1

      GeneratedRecordType(
        recordType=recordType,
        irType=userDefinedType,
        classId=classId,
        fieldToTbaaIndex=fieldToTbaaIndex,
        storageType=storageType)
    })
  }
}
