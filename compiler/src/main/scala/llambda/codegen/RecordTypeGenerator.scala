package llambda.codegen

import llambda.{valuetype => vt}
import llambda.codegen.llvmir._

case class GeneratedRecordType(irType : UserDefinedType, classId : Long)

class RecordTypeGenerator(module : IrModuleBuilder) {
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

      // Allocate it a class ID
      val classId = nextClassId
      nextClassId = nextClassId + 1

      GeneratedRecordType(userDefinedType, classId)
    })
  }
}
