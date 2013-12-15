package io.llambda.typegen

import collection.immutable.ListMap
import scala.util.parsing.input.Positional

import io.llambda.llvmir

/** Defined field inside a class cell
  *
  * This is a wrapper to ensure fields with the same type aren't considered
  * equal
  */
final class CellField(
  val fieldType : FieldType
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

sealed abstract class CellClass extends Positional {
  val name : String
  val instanceType : CellClass.InstanceType
  val fields : ListMap[String, CellField]
  val internal : Boolean
  val typeId : Option[Int]
  val optionalParent : Option[CellClass]
  
  // This also contains the TBAA nodes for the fields we inherit
  val fieldTbaaNodes : Map[CellField, llvmir.IrTbaaNode]

  lazy val names = CellClassNames(name)
}

case class RootCellClass(name : String, typeTagField : CellField, fields : ListMap[String, CellField], internal : Boolean, fieldTbaaNodes : Map[CellField, llvmir.IrTbaaNode]) extends CellClass {
  val instanceType = CellClass.Abstract
  val typeId = None
  val optionalParent = None
}

case class ChildCellClass(name : String, instanceType : CellClass.InstanceType, parent : CellClass, fields : ListMap[String, CellField], internal : Boolean, typeId : Option[Int], fieldTbaaNodes : Map[CellField, llvmir.IrTbaaNode]) extends CellClass {
  val optionalParent = Some(parent)
}
