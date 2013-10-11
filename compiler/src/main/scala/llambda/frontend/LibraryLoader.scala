package llambda.frontend

import llambda._

abstract class LibraryLoader {
  def load(libraryName : Seq[LibraryNameComponent]) : Map[String, BoundValue]
  
  def libraryExpressions : List[et.Expression]
}
