package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

private[frontend] object ParseLibraryName {
  def apply(datum : ast.Datum) : List[LibraryNameComponent] = datum match {
    case ast.ProperList(firstComponent :: restComponent) =>
      (firstComponent :: restComponent) map {
        case ast.Symbol(str) => StringComponent(str)
        case ast.IntegerLiteral(int) if int > 0 => IntegerComponent(int)
        case otherComponent => throw new InvalidLibraryNameException(otherComponent) 
      }

    case _ => throw new InvalidLibraryNameException(datum)
  }
}
