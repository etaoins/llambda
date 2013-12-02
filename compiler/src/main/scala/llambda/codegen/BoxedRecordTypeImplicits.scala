package llambda.codegen

import llambda.InternalCompilerErrorException
import llambda.{valuetype => vt}

object BoxedRecordTypeImplicits {
  implicit class BoxedRecordTypeHelper(recordType : vt.BoxedRecordType) {
    def indexOfField(recordField : vt.RecordField) : Int = 
      recordType.fields.zipWithIndex.find({ case (field, index) =>
        field == recordField
      }).map(_._2).getOrElse({
        throw new InternalCompilerErrorException("Attempted to retrieve index of field not in record type")
      })
  }
}

