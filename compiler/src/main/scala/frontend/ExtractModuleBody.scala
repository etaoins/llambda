package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

/** Function object for extracting expressions at the outermost level */
private object ExtractOutermostExpr extends ExprExtractor {
  private def guardOutermostDefinition(symbol : sst.ScopedSymbol)(implicit context : FrontendContext) {
    if (!context.config.schemeDialect.allowTopLevelRedefinition && symbol.resolveOpt.isDefined) {
      throw new DuplicateDefinitionException(symbol)
    }
  }

  protected def handleParsedDefine(
      located : SourceLocated,
      parsedDefine : ParsedDefine
  )(implicit context : FrontendContext) : et.Expr = parsedDefine match {
    case ParsedVarDefine(symbol, providedTypeOpt, exprBlock, storageLocConstructor) =>
      // There's a wart in Scheme that allows a top-level (define) to become a (set!) if the value is already defined as
      // a storage location
      symbol.resolveOpt match {
        case Some(_) if !context.config.schemeDialect.allowTopLevelRedefinition =>
          throw new DuplicateDefinitionException(symbol)

        case Some(storageLoc : StorageLocation) =>
          // Convert this to a (set!)
          et.MutateVar(storageLoc, exprBlock())

        case Some(_) =>
          throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}")

        case None =>
          // This is a fresh binding
          val schemeType = SchemeTypeForSymbol(symbol, providedTypeOpt)
          val boundValue = storageLocConstructor(symbol.name, schemeType)

          symbol.scope += (symbol.name -> boundValue)
          et.TopLevelDefine(List(et.SingleBinding(boundValue, exprBlock())))
      }

    case ParsedMultipleValueDefine(fixedValueTargets, restValueTargetOpt, exprBlock) =>
      // Don't support re-defining top-level values with (define-values) in any dialect
      for (symbol <- (fixedValueTargets ++ restValueTargetOpt).map(_.definedSymbol)) {
        if (symbol.resolveOpt.isDefined) {
          throw new DuplicateDefinitionException(symbol)
        }
      }

      val fixedLocs = fixedValueTargets.map(_.createStorageLoc(vt.AnySchemeType))
      val restLocOpt = restValueTargetOpt.map(_.createStorageLoc(vt.UniformProperListType(vt.AnySchemeType)))

      et.TopLevelDefine(List(et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock())))

    case ParsedSimpleDefine(symbol, boundValue) =>
      guardOutermostDefinition(symbol)

      // This doesn't create any expression tree nodes
      symbol.scope += (symbol.name -> boundValue)
      et.Begin(Nil)

    case ParsedRecordTypeDefine(typeSymbol, recordType, procedures) =>
      typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

      et.TopLevelDefine((procedures.map { case (procedureSymbol, expr) =>
        val schemeType = SchemeTypeForSymbol(procedureSymbol)
        val storageLoc = new StorageLocation(procedureSymbol.name, schemeType)

        guardOutermostDefinition(procedureSymbol)

        procedureSymbol.scope += (procedureSymbol.name -> storageLoc)

        et.SingleBinding(storageLoc, expr)
      }).toList)

    case ParsedTypeAnnotation =>
      et.Begin(Nil)
  }
}

object ExtractModuleBody  {
  /** Converts the body of a module (library or program) to a list of expressions
    *
    * @param  data       Scheme date of the module's body
    * @param  evalScope  Scope to evaluate the body in
    */
  def apply(data : List[ast.Datum], evalScope : Scope)(implicit context : FrontendContext) : List[et.Expr] =
    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)

      ExpandBodyDatum(scopedDatum).flatMap {
        ExtractOutermostExpr(_).toSequence
      }
    }
}
