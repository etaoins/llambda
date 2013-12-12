package io.llambda.typegen

/** Common types related to cell classes */
object CellClass {
  /** Instance type of a cell class */
  sealed abstract class InstanceType
  
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

