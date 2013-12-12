package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

private[frontend] object ResolveIncludeList {
  def apply(includeNameData : List[ast.Datum], includeLocation : SourceLocated = NoSourceLocation)(implicit includePath : IncludePath) : List[IncludeLoadResult] = {
    if (includeNameData.isEmpty) {
      throw new BadSpecialFormException(includeLocation, "At least one file required for include")
    }

    // All include names must be strings
    includeNameData map {
      case includeLiteral @ ast.StringLiteral(includeName) =>
        IncludeLoader(includePath.includeSearchRoots, includeName) match {
          case Some(result) => result
          case _ =>
            throw new IncludeNotFoundException(includeLiteral, includeName)
        }
      case other =>
        throw new BadSpecialFormException(other, "Attempted include with non-string argument")
    }
  }
}

