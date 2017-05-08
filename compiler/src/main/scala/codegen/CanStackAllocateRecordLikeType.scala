package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{valuetype => vt}


object CanStackAllocateRecordLikeType {
  /** Determine if a record-like type can be allocated on the stack
    *
    * This is currently limited to record types with inline data. This is conservative as it does not take in to account
    * the size of the fields or platform-specific record packing.
    */
  def apply(recordLikeType: vt.RecordLikeType): Boolean = recordLikeType match {
    case recordType: vt.RecordType =>
      // We have 16 bytes of extra data + one pointer. This is at least 20 bytes so we can pack at least two 64bit
      // values.
      recordType.fieldsWithInherited.size <= 2

    case closureType: vt.ClosureType =>
      // We have 8 bytes of extra data + one pointer. This is at least 12 bytes so we can pack at least one 64bit
      // value.
      closureType.fieldsWithInherited.size <= 1
  }
}
