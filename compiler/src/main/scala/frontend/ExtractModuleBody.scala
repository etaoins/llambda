package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.frontend.syntax.ExpandMacro

object ExtractModuleBody {
  private def extractInclude(
      located: SourceLocated,
      scope: Scope,
      includeNameData: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): List[et.Expr] = {
    val includeData = ResolveIncludeList(located, includeNameData.map(_.unscope))(context.config.includePath)

    ExtractModuleBody(includeData, scope)
  }

  private def handleExtractedDefine(
      located: SourceLocated,
      extractedDefine: ExtractedVarDefine
  )(implicit context: FrontendContext): List[et.Expr] = extractedDefine match {
    case ExtractedVarDefine(valueTarget, exprBlock) =>
      val symbol = valueTarget.definedSymbol

      symbol.resolveOpt match {
        case Some(_) =>
          throw new DuplicateDefinitionException(symbol)

        case None =>
          // This is a fresh binding
          val boundValue = valueTarget.bindStorageLoc(vt.AnySchemeType)
          List(et.TopLevelDefine(et.Binding(boundValue, exprBlock())))
      }
  }

  private def extractOutermostExpr(
      datum: sst.ScopedDatum
  )(implicit context: FrontendContext): List[et.Expr] = datum match {
    case sst.Pair(appliedSymbol: sst.Symbol, cdr) =>
      (appliedSymbol.resolve, cdr) match {
        case (syntax: BoundSyntax, _) =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)
          extractOutermostExpr(expandedDatum)

        case (Primitives.Begin, sst.ProperList(innerExprData)) =>
          // This is a (begin) - flatten it
          innerExprData.flatMap(extractOutermostExpr)

        case (Primitives.Include, sst.ProperList(includeNames)) =>
          // We need the scope from the (include) to rescope the included file
          val scope = appliedSymbol.scope
          extractInclude(appliedSymbol, scope, includeNames)

        case (definePrimitive: PrimitiveDefineExpr, sst.ProperList(operands)) =>
          ExtractDefine(datum, definePrimitive, operands).flatMap { define =>
            handleExtractedDefine(datum, define)
          }

        case _ =>
          ExtractExpr(datum).toSequence
      }

    case _ =>
      ExtractExpr(datum).toSequence
  }

  /** Converts the body of a module (library or program) to a list of expressions
    *
    * @param  data       Scheme date of the module's body
    * @param  evalScope  Scope to evaluate the body in
    */
  def apply(data: List[ast.Datum], evalScope: Scope)(implicit context: FrontendContext): List[et.Expr] =
    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)
      extractOutermostExpr(scopedDatum)
    }
}
