package io.llambda.compiler.frontend
import io.llambda

import java.net.URL
import java.io.File

import llambda.compiler._

private[frontend] object ResolveIncludeList {
  /** Resolves a list of files to include to a corresponding list of included files
    *
    * @param  located          Source location of the include. This is to support both relative includes and to report
    *                          errors
    * @param  includeNameData  User supplied body of the (include). Currently only lists of one or more strings are
    *                          supported; all other input will raise a BadSpecialFormException
    * @return Include load results for the include name data
    */
  def apply(
      located: SourceLocated,
      includeNameData: List[ast.Datum]
  )(implicit includePath: IncludePath): List[ast.Datum] = {
    if (includeNameData.isEmpty) {
      throw new BadSpecialFormException(located, "At least one file required for include")
    }

    // If we have a source location filename add that to our include path
    val relativeIncludeRootOpt = located.locationOpt.flatMap(_.filenameOpt).map { filename =>
      new URL(new File(filename).toURI.toURL, ".")
    }

    val allSearchRoots = relativeIncludeRootOpt.toList ++ includePath.includeSearchRoots

    // All include names must be strings
    includeNameData flatMap {
      case includeLiteral @ ast.StringLiteral(includeName) =>
        IncludeLoader(allSearchRoots, includeName) match {
          case Some(result) => result
          case _ =>
            throw new IncludeNotFoundException(includeLiteral, includeName)
        }
      case other =>
        throw new BadSpecialFormException(other, "Attempted include with non-string argument")
    }
  }
}

