package llambda.testutil

import llambda._

object TestLibraryLoader extends frontend.LibraryLoader {
  def load(libraryName : Seq[LibraryNameComponent]) : Map[String, BoundValue] = libraryName match {
    case StringComponent("test") :: StringComponent("primitives") :: Nil =>
      SchemePrimitives.bindings
  }

  def libraryExpressions : List[et.Expression] = Nil 
}
