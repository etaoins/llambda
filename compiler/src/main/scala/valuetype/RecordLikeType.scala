package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type can be distinct if it's declared in
  * another record type.
  */
class RecordField(val name : String, val fieldType : ValueType)

/** Trait for record-like types
  *
  * Record-like types contain a number of typed fields and an optional parent type. They are used to implement both
  * Scheme records and procedure closures
  */
abstract trait RecordLikeType {
  val cellType : ct.ConcreteCellType with ct.RecordLikeFields

  val sourceName : String
  val parentRecordLikeOpt : Option[RecordLikeType]
  val fields : List[RecordField]

  /** Returns the fields we inherit from our parent recursively */
  def inheritedFields : List[RecordField] =
    parentRecordLikeOpt.map(_.fieldsWithInherited).getOrElse(Nil)

  /** Returns the fields we introduced followed be the fields we inherit from our parent */
  def fieldsWithInherited : List[RecordField] =
    fields ++ inheritedFields
}

/** Pointer to a closure type
  *
  * Closure types store the data needed for a procedure from its parent lexical scope. The storage is internally
  * implemented identically to user-defined record types.
  */
class ClosureType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType {
  val cellType = ct.ProcedureCell
  val parentRecordLikeOpt = None
}
