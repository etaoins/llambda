package llambda

sealed abstract class LibraryNameComponent

case class StringComponent(value : String) extends LibraryNameComponent {
  override def toString = value
}

case class IntegerComponent(value : Long) extends LibraryNameComponent {
  override def toString = value.toString
}

case class Library(name : List[LibraryNameComponent], exports : Map[String, BoundValue], expressions : List[et.Expression])
