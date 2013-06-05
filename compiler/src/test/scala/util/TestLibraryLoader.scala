package llambda.testutil

import llambda._

object TestLibraryLoader {
  def apply(libraryName : Seq[LibraryNameComponent]) : Map[String, BoundValue] = libraryName match {
    case StringComponent("test") :: StringComponent("primitives") :: Nil =>
      SchemePrimitives.bindings
  }
}


