package llambda.frontend

import java.net.URL
import llambda.ast

private[frontend] case class IncludeLoadResult(
  innerIncludePath : IncludePath,
  data : List[ast.Datum]
)
