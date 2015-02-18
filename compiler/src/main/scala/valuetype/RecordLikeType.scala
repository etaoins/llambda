package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type can be distinct if it's declared in
  * another record type.
  *
  * @param  name          Source name of the record field
  * @param  typeTemplate  Template for the field type. This is expanded to include any recursive type references
  * @param  mutable       Indicates if this record field is mutable. If all of a records fields are immutable its a
  *                       candidate to be constructed as a global constant.
  */
class RecordField(val name : String, val typeTemplate : ValueType, val mutable : Boolean)

/** Trait for record-like types
  *
  * Record-like types contain a number of typed fields and an optional parent type. They are used to implement both
  * Scheme records and procedure closures
  */
abstract trait RecordLikeType {
  val cellType : ct.ConcreteCellType with ct.RecordLikeFields

  /** Souce name for this record-like. This is used when printing type names and when generating LLVM IR */
  val sourceName : String

  /* Optional parent RecordType for to support inheritance */
  val parentRecordOpt : Option[RecordLikeType]

  val fields : List[RecordField]

  /** Returns the instantiated type for the given record field */
  val typeForField : Map[RecordField, ValueType]

  /** Returns the fields we inherit from our parent recursively */
  def inheritedFields : List[RecordField] =
    parentRecordOpt.map(_.fieldsWithInherited).getOrElse(Nil)

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
  val parentRecordOpt = None

  lazy val typeForField = (fields.map { field =>
    // Closures don't support recursion or polymorphism - short circuit this
    field -> field.typeTemplate
  }).toMap
}
