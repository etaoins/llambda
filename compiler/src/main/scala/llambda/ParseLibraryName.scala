package llambda

case class InvalidLibraryNameException(val datum : ast.Datum) 
  extends SemanticException(datum.toString)

object ParseLibraryName {
  def apply(datum : ast.Datum) : Seq[LibraryNameComponent] = datum match {
    case ast.ProperList(firstComponent :: restComponent) =>
      (firstComponent :: restComponent) map {
        case ast.Symbol(str) => StringComponent(str)
        case ast.IntegerLiteral(int) if int > 0 => IntegerComponent(int)
        case otherComponent => throw new InvalidLibraryNameException(datum) 
      }

    case _ => throw new InvalidLibraryNameException(datum)
  }
}
