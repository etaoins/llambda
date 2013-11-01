package llambda.frontend

import llambda._

private[frontend] object ResolveIncludeList {
  def apply(includeNameData : List[ast.Datum])(implicit includePath : IncludePath) : List[IncludeLoadResult] = {
    if (includeNameData.isEmpty) {
      throw new BadSpecialFormException("At least one file required for include")
    }

    // All include names must be strings
    val includeNames = includeNameData collect {
      case ast.StringLiteral(string) => string
      case _ =>
        throw new BadSpecialFormException("Attempted include with non-string argument")
    }
    
    includeNames.map { includeName =>
      IncludeLoader(includePath.includeSearchRoots, includeName) match {
        case Some(result) => result
        case _ =>
          throw new IncludeNotFoundException(includeName)
      }
    }
  }
}

