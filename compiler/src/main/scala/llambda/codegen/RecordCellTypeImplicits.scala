package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.{valuetype => vt}

object RecordCellTypeImplicits {
  implicit class RecordCellTypeHelper(recordType : vt.RecordCellType) {
    def indexOfField(recordField : vt.RecordField) : Int = 
      recordType.fields.zipWithIndex.find({ case (field, index) =>
        field == recordField
      }).map(_._2).getOrElse({
        throw new InternalCompilerErrorException("Attempted to retrieve index of field not in record type")
      })
  }
}

