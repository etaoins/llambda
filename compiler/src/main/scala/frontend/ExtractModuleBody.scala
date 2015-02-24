package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.frontend.syntax.ExpandMacro

object ExtractModuleBody  {
  private def guardOutermostDefinition(symbol : sst.ScopedSymbol)(implicit context : FrontendContext) {
    if (!context.config.schemeDialect.allowTopLevelRedefinition && symbol.resolveOpt.isDefined) {
      throw new DuplicateDefinitionException(symbol)
    }
  }

  protected def handleParsedDefine(
      located : SourceLocated,
      parsedDefine : ParsedDefine
  )(implicit context : FrontendContext) : List[et.Expr] = parsedDefine match {
    case ParsedVarDefine(symbol, providedTypeOpt, exprBlock, storageLocConstructor) =>
      // There's a wart in Scheme that allows a top-level (define) to become a (set!) if the value is already defined as
      // a storage location
      symbol.resolveOpt match {
        case Some(_) if !context.config.schemeDialect.allowTopLevelRedefinition =>
          throw new DuplicateDefinitionException(symbol)

        case Some(storageLoc : StorageLocation) =>
          // Convert this to a (set!)
          List(et.MutateVar(storageLoc, exprBlock()))

        case Some(_) =>
          throw new BadSpecialFormException(symbol, s"Attempted mutating (define) non-variable ${symbol.name}")

        case None =>
          // This is a fresh binding
          val schemeType = SchemeTypeForSymbol(symbol, providedTypeOpt)
          val boundValue = storageLocConstructor(symbol.name, schemeType)

          symbol.scope += (symbol.name -> boundValue)
          List(et.TopLevelDefine(List(et.SingleBinding(boundValue, exprBlock()))))
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

      List(et.TopLevelDefine(List(et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock()))))

    case ParsedSimpleDefine(symbol, boundValue) =>
      guardOutermostDefinition(symbol)

      // This doesn't create any expression tree nodes
      symbol.scope += (symbol.name -> boundValue)
      Nil

    case ParsedRecordTypeDefine(typeSymbol, recordType, procedures) =>
      typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

      List(et.TopLevelDefine((procedures.map { case (procedureSymbol, expr) =>
        val schemeType = SchemeTypeForSymbol(procedureSymbol)
        val storageLoc = new StorageLocation(procedureSymbol.name, schemeType)

        guardOutermostDefinition(procedureSymbol)

        procedureSymbol.scope += (procedureSymbol.name -> storageLoc)

        et.SingleBinding(storageLoc, expr)
      }).toList))

    case ParsedTypeAnnotation =>
      Nil
  }

  private def extractOutermostExpr(
      datum : sst.ScopedDatum
  )(implicit context : FrontendContext) : List[et.Expr] = datum match {
    case sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr) =>
      (appliedSymbol.resolve, cdr) match {
        case (syntax : BoundSyntax, _) =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)
          extractOutermostExpr(expandedDatum)

        case (Primitives.Begin, sst.ScopedProperList(innerExprData)) =>
          // This is a (begin) - flatten it
          innerExprData.flatMap(extractOutermostExpr)

        case (definePrimitive : PrimitiveDefineExpr, sst.ScopedProperList(operands)) =>
          handleParsedDefine(datum, ParseDefine(datum, definePrimitive, operands))

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
  def apply(data : List[ast.Datum], evalScope : Scope)(implicit context : FrontendContext) : List[et.Expr] =
    data flatMap { datum =>
      // Annotate our symbols with our current scope
      val scopedDatum = sst.ScopedDatum(evalScope, datum)
      extractOutermostExpr(scopedDatum)
    }
}
