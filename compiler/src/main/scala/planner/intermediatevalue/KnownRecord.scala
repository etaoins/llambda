package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}

trait KnownRecord {
  /** Map of fields with known values
    *
    * This may contain only a subset of the record's fields. For example, this will not contain any mutable fields.
    */
  val knownFieldValues : Map[vt.RecordField, IntermediateValue]
}
