package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.SourceLocated
import llambda.compiler.et
import llambda.compiler.DefinitionOutsideTopLevelException

/** Function object for extracting expressions in an inner (i.e. not outermost) level */
object ExtractInnerExpr extends ExprExtractor {
  protected def handleParsedDefine(
      located : SourceLocated,
      parsedDefine : ParsedDefine
  )(implicit context : FrontendContext) : et.Expr =
    throw new DefinitionOutsideTopLevelException(located)
}
