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
  
  /** Abstract root of all cell classes */
  object Root extends InstanceType
  
  /** Cell class is concrete; it can be instanciated at runtime */
  object Concrete extends InstanceType
  
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

  val instanceType : CellClass.InstanceType

  /** Ordered map of cell class fields indexed by the field name
    *
    * This does not include fields inherited from the parent class
    */
  val fields : ListMap[String, CellField]

  /** Indicates if this cell class should be hidden from Scheme
    *
    * If this is true then no Scheme type value will be registered in
    * IntrinsicCellTypes and no type predicate will be generated
    */
  val internal : Boolean

  /** Automatically assigned type ID for non-abstract cell classes */
  val typeId : Option[Int]

  /** Optional parent of the cell class
    *
    * Only the root cell class does not have a parent
    */
  val parentOption : Option[CellClass]
  
  /** TBAA nodes for the cell's fields, including inherited fields */
  val fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]

  /** Alternative names for the cell class for various output format */
  lazy val names = CellClassNames(name)
}
/** Root class for all other cell classes
  * 
  * There is only one root cell class for any set of cell class definitions
  */
case class RootCellClass(name : String, typeTagField : CellField, fields : ListMap[String, CellField], internal : Boolean, fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]) extends CellClass {
  val instanceType = CellClass.Abstract
  val typeId = None
  val parentOption = None
}

/** Non-root cell class */
case class ChildCellClass(name : String, instanceType : CellClass.InstanceType, parent : CellClass, fields : ListMap[String, CellField], internal : Boolean, typeId : Option[Int], fieldTbaaNodes : ListMap[CellField, llvmir.IrTbaaNode]) extends CellClass {
  val parentOption = Some(parent)
}
