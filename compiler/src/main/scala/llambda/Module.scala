package llambda

sealed abstract class LibraryNameComponent
case class StringComponent(value : String) extends LibraryNameComponent
case class IntegerComponent(value : Int) extends LibraryNameComponent
