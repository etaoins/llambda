package llambda

sealed abstract class LibraryNameComponent
case class StringComponent(value : String) extends LibraryNameComponent
case class IntegerComponent(value : Int) extends LibraryNameComponent

case class Library(name : List[LibraryNameComponent], exports : Map[String, BoundValue], expressions : List[et.Expression])
