package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type can be distinct if it's declared in
  * another record type.
  */
class RecordField(val name : String, val typeTemplate : ValueType)

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
  val parentRecordOpt : Option[RecordTypeInstance]

  val fields : List[RecordField]

  /** Returns the physical storage type for each field including inherited fields */
  val storageTypeForField : Map[RecordField, ValueType]

  /** Returns the fields we inherit from our parent recursively */
  def inheritedFields : List[RecordField] =
    parentRecordOpt.map(_.recordType.fieldsWithInherited).getOrElse(Nil)

  /** Returns the fields we introduced followed be the fields we inherit from our parent */
  def fieldsWithInherited : List[RecordField] =
    fields ++ inheritedFields

  val upperBound : RecordLikeTypeInstance
}

abstract trait RecordLikeTypeInstance {
  /** Returns the logical Scheme type for the given record field including inherited fields */
  val schemeTypeForField : Map[RecordField, SchemeType]
}

class RecordType(
    val sourceName : String,
    val fields : List[RecordField],
    val selfTypeVarOpt : Option[pm.TypeVar] = None,
    val typeVars : List[pm.TypeVar] = Nil,
    val parentRecordOpt : Option[RecordTypeInstance] = None
) extends RecordLikeType {
  val cellType = ct.RecordCell

  /** Test if this type is equal to or a child of another type */
  def isEqualToOrChildOf(other : RecordType) : Boolean = {
    if (other eq this) {
      true
    }
    else {
      parentRecordOpt match {
        case Some(parentInstance) =>
          parentInstance.recordType.isEqualToOrChildOf(other)

        case None =>
          false
      }
    }
  }

  lazy val upperBound : RecordTypeInstance = {
    val reconciledVars = pm.ReconcileTypeVars(typeVars.toSet)
    RecordTypeInstance(reconciledVars, this)
  }

  lazy val storageTypeForField : Map[RecordField, ValueType] = {
    val selfTypeVars = selfTypeVarOpt.map(_ -> this.upperBound).toMap
    val resolvedVars = pm.ResolveTypeVars.Result(selfTypeVars)

    val allTypeVars = typeVars.toSet ++ selfTypeVarOpt
    val reconciledVars = pm.ReconcileTypeVars(allTypeVars, resolvedVars)

    fields.map({ field =>
      field -> pm.InstantiateType(reconciledVars, field.typeTemplate)
    }).toMap ++ parentRecordOpt.map(_.recordType.storageTypeForField).getOrElse(Map())
  }
}

/** Pointer to a closure type
  *
  * Closure types store the data needed for a procedure from its parent lexical scope. The storage is internally
  * implemented identically to user-defined record types.
  */
class ClosureType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType {
  val cellType = ct.ProcedureCell
  val parentRecordOpt = None

  // Closures don't support recursion or polymorphism - short circuit this
  lazy val typeForField = (fields.map { field =>
    field -> field.typeTemplate
  }).toMap

  lazy val storageTypeForField = typeForField

  lazy val upperBound = ClosureTypeInstance(this)
}

case class ClosureTypeInstance(closureType : ClosureType) extends RecordLikeTypeInstance {
  val schemeTypeForField = closureType.typeForField.mapValues(_.schemeType)
}
