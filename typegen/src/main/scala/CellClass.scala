package io.llambda.typegen

import collection.immutable.ListMap
import scala.util.parsing.input.Positional

import io.llambda.llvmir

/** Defined field inside a cell class */
final class CellField(
  val name : String,
  val fieldType : FieldType,
  val initializer : Option[Long]
) extends Positional


/** Common types related to cell classes */
object CellClass {
  /** Instance type of a cell class */
  sealed abstract class InstanceType
  
  /** Cell class is concrete; it can be instanciated at runtime */
  object Concrete extends InstanceType

  /** Cell clas is a layout variant of a non-abstract cell class */
  object Variant extends InstanceType
  
  /** Cell class is abstract; it is only used as a base type */
  object Abstract extends InstanceType

  /** Cell class has all of its instances built at compile time
    *
    * This is used by cell classes with low numbers of possible instances (empty
    * list, boolean, etc) to reduce GC pressure
    */
  object Preconstructed extends InstanceType
}

/** Top-level types for cell classes
  *
  * Cell classes contain information about the structure of a cons cell in memory
  */
sealed abstract class CellClass extends Positional {
  /** Base name of the cell class
    *
    * This is the name used in definition file. All other names are derived from 
    * this name by the [[CellClassNames]] class
    */
  val name : String
  
  /** List of fields in the order they appear in memory */
  val fields : List[CellField]

  /** TBAA nodes for the cell's fields, including inherited fields */
  val fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]

  /** Alternative names for the cell class in multiple output formats */
  lazy val names = CellClassNames(name)
  
  val instanceType : CellClass.InstanceType

  /** Indicates if this cell class should be hidden from Scheme
    *
    * If this is true then no Scheme type value will be registered in
    * IntrinsicCellTypes and no type predicate will be generated
    */
  val internal : Boolean

  /** Optional parent of the cell class
    *
    * Only the root cell class does not have a parent
    */
  val parentOption : Option[CellClass]
  
  /** Automatically assigned type ID for non-abstract cell classes */
  val typeId : Option[Int]
}

/** Cell class with a parent
  *
  * This is all cell class types except for RootCellClass
  */
sealed abstract class ParentedCellClass extends CellClass {
  val parent : CellClass
  val parentOption = Some(parent)
}

/** Root class for all other cell classes
  * 
  * There is only one root cell class for any set of cell class definitions
  */
case class RootCellClass(
    name : String,
    typeTagField : CellField,
    fields : List[CellField],
    internal : Boolean,
    fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]
  ) extends CellClass {
  val instanceType = CellClass.Abstract
  val typeId = None
  val parentOption = None
}

/** Type tagged cell class */
case class TaggedCellClass(
    name : String,
    instanceType : CellClass.InstanceType,
    parent : CellClass,
    fields : List[CellField],
    internal : Boolean,
    typeId : Option[Int],
    fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]
  ) extends ParentedCellClass

/** Variant of a type tagged cell class 
  *
  * Variants can only be distinguished from each other at runtime; the type
  * tag isn't sufficient
  */
case class VariantCellClass(
    name : String,
    parent : CellClass,
    fields : List[CellField],
    fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]
  ) extends ParentedCellClass {
  val instanceType = CellClass.Variant
  val internal = true
  val typeId = None
}
