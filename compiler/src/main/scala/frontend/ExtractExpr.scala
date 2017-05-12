package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.frontend.syntax.ExpandMacro


private[frontend] object ExtractExpr {
  private def extractInclude(
      located: SourceLocated,
      scope: Scope,
      includeNameData: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): et.Expr = {
    val includeData = ResolveIncludeList(located, includeNameData.map(_.unscope))(context.config.includePath)

    val scopedData = includeData.map(sst.ScopedDatum(scope, _))
    ExtractBodyDefinition(Nil, scopedData)
  }

  private def extractNonDefineApplication(
      boundValue: BoundValue,
      appliedSymbol: sst.Symbol,
      operands: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): et.Expr = {
    (boundValue, operands) match {
      case (storageLoc: StorageLocation, args) =>
        et.Apply(
          et.VarRef(storageLoc).assignLocationAndContextFrom(appliedSymbol, context.debugContext),
          args.map(ExtractExpr.apply)
        )

      case (Primitives.Begin, exprData) =>
        ExtractBodyDefinition(Nil, exprData)

      case (Primitives.Quote, innerDatum :: Nil) =>
        et.Literal(innerDatum.unscope)

      case (Primitives.If, test :: trueExpr :: falseExpr :: Nil) =>
        et.Cond(
          ExtractExpr(test),
          ExtractExpr(trueExpr),
          ExtractExpr(falseExpr))

      case (Primitives.If, test :: trueExpr :: Nil) =>
        et.Cond(
          ExtractExpr(test),
          ExtractExpr(trueExpr),
          et.Literal(ast.Unit()).assignLocationAndContextFrom(appliedSymbol, context.debugContext)
        )

      case (Primitives.Set, (mutatingSymbol: sst.Symbol) :: value :: Nil) =>
        mutatingSymbol.resolve match {
          case storageLoc: StorageLocation =>
            if (storageLoc.forceImmutable) {
              throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) of immutable binding ${mutatingSymbol.name}")
            }
            else {
              et.MutateVar(storageLoc, ExtractExpr(value))
            }

          case _ =>
            throw new BadSpecialFormException(mutatingSymbol, s"Attempted (set!) non-variable ${mutatingSymbol.name}")
        }

      case (Primitives.Lambda, sst.ListOrDatum(fixedArgData, restArgDatum) :: definition) =>
        ExtractLambda(
          located=appliedSymbol,
          argList=fixedArgData,
          argTerminator=restArgDatum,
          definition=definition
        )

      case (Primitives.CaseLambda, clauseData) =>
        ExtractCaseLambda(
          located=appliedSymbol,
          clauseData=clauseData
        )

      case (Primitives.SyntaxError, (errorDatum @ sst.NonSymbolLeaf(ast.String(errorString))) :: data) =>
        throw new UserDefinedSyntaxError(errorDatum, errorString, data.map(_.unscope))

      case (Primitives.Include, includeNames) =>
        // We need the scope from the (include) to rescope the included file
        val scope = appliedSymbol.scope
        extractInclude(appliedSymbol, scope, includeNames)

      case (Primitives.NativeFunction, _) =>
        ExtractNativeFunction(appliedSymbol, false, operands)

      case (Primitives.WorldFunction, _) =>
        ExtractNativeFunction(appliedSymbol, true, operands)

      case (Primitives.Quasiquote, sst.ProperList(listData) :: Nil) =>
        val schemeBase = context.libraryLoader.loadSchemeBase(context.config)
        (new ListQuasiquotationExpander(ExtractExpr.apply, schemeBase))(listData)

      case (Primitives.Quasiquote, sst.Vector(elements) :: Nil) =>
        val schemeBase = context.libraryLoader.loadSchemeBase(context.config)
        (new VectorQuasiquotationExpander(ExtractExpr.apply, schemeBase))(elements.toList)

      case (Primitives.Unquote, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote) outside of quasiquotation")

      case (Primitives.UnquoteSplicing, _) =>
        throw new BadSpecialFormException(appliedSymbol, "Attempted (unquote-splicing) outside of quasiquotation")

      case (Primitives.Cast, valueExpr :: typeDatum :: Nil) =>
        et.Cast(ExtractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), false)

      case (Primitives.AnnotateExprType, valueExpr :: typeDatum :: Nil) =>
        et.Cast(ExtractExpr(valueExpr), ExtractType.extractSchemeType(typeDatum), true)

      case (Primitives.CondExpand, firstClause :: restClauses) =>
        val expandedData = CondExpander.expandScopedData(firstClause :: restClauses)(context.libraryLoader, context.config)

        et.Begin(expandedData.map(ExtractExpr.apply))

      case (Primitives.Parameterize, sst.ProperList(parameterData) :: bodyData) =>
        val parameters = parameterData map { parameterDatum =>
          parameterDatum match {
            case sst.ProperList(List(parameter, value)) =>
              (ExtractExpr(parameter), ExtractExpr(value))

            case _ =>
              throw new BadSpecialFormException(parameterDatum, "Parameters must be defined as (parameter value)")
          }
        }

        et.Parameterize(
          parameterValues=parameters,
          ExtractBodyDefinition(Nil, bodyData)
        )

      case (Primitives.MakePredicate, List(typeDatum)) =>
        val schemeType = ExtractType.extractSchemeType(typeDatum)
        et.TypePredicate(schemeType)

      case (Primitives.PatternMatch, valueDatum :: clauseData) =>
        val valueExpr = ExtractExpr(valueDatum)
        ExtractPatternMatch(valueExpr, clauseData)

      case otherPrimitive =>
        throw new BadSpecialFormException(appliedSymbol, "Invalid primitive syntax")
    }
  }

  /** Extract an expression from an application
    *
    * @param  boundValue     Bound value being applied
    * @param  appliedSymbol  Symbol being applied. This is used to locate exceptions and scope includes
    * @param  operands       Operands for the symbol application
    */
  private def extractApplication(
      boundValue: BoundValue,
      appliedSymbol: sst.Symbol,
      operands: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): et.Expr = boundValue match {
    case primitiveDefine: PrimitiveDefineExpr =>
      throw new DefinitionOutsideTopLevelException(appliedSymbol)

    case _ =>
      // Apply the symbol
      // This is the only way to "apply" syntax and primitives
      // They cannot appear as normal expression values
      extractNonDefineApplication(boundValue, appliedSymbol, operands)
  }

  /** Extracts an expression from the passed scoped data
    *
    * If definitions are allowed in this level they should be handled before calling this method
    */
  def apply(datum: sst.ScopedDatum)(implicit context: FrontendContext): et.Expr = (datum match {
    case sst.Pair(appliedSymbol: sst.Symbol, cdr) =>
      appliedSymbol.resolve match {
        case syntax: BoundSyntax =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, datum, trace=context.config.traceMacroExpansion)

          // Get the expanded expression
          val expandedContext = context.copy(debugContext=syntax.debugContext)

          // Use ourselves to expand this datum. This preserves our "outermostness" if we're at the outermost level
          val expandedExpr = apply(expandedDatum)(expandedContext)

          // Mark our expansion in the inline path
          // This is used for debug info and error reporting
          val inlinePathEntry = InlinePathEntry(
            locationOpt=appliedSymbol.locationOpt,
            contextOpt=Some(context.debugContext),
            inlineReason=InlineReason.MacroExpansion
          )

          expandedExpr.asInlined(inlinePathEntry)

        case otherBoundValue =>
          // Make sure the operands are a proper list
          // XXX: Does R7RS only allow macros to be applied as an improper list?
          cdr match {
            case sst.ProperList(operands) =>
              extractApplication(otherBoundValue, appliedSymbol, operands)

            case improperList =>
              throw new MalformedExprException(improperList, "Non-syntax cannot be applied as an improper list")
          }
      }

    case sst.ProperList(procedure :: args) =>
      // Apply the result of the inner expression
      val procedureExpr = ExtractExpr(procedure)
      et.Apply(procedureExpr, args.map(ExtractExpr.apply))

    case scopedSymbol: sst.Symbol =>
      scopedSymbol.resolve match {
        case storageLoc: StorageLocation =>
          et.VarRef(storageLoc)

        case _: BoundSyntax =>
          throw new MalformedExprException(scopedSymbol, "Syntax cannot be used as an expression")

        case _: PrimitiveExpr =>
          throw new MalformedExprException(scopedSymbol, "Primitive cannot be used as an expression")

        case _: TypeConstructor =>
          throw new MalformedExprException(scopedSymbol, "Type constructor cannot be used as an expression")

        case _: BoundType =>
          throw new MalformedExprException(scopedSymbol, "Type cannot be used as an expression")

        case _: NativeLibrary =>
          throw new MalformedExprException(scopedSymbol, "Native library cannot be used as an expression")
      }

    // These all evaluate to themselves. See R7RS section 4.1.2
    case literal: sst.Vector =>
      et.Literal(literal.unscope)
    case sst.NonSymbolLeaf(literal: ast.Number) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal: ast.String) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal: ast.Char) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal: ast.Bytevector) =>
      et.Literal(literal)
    case sst.NonSymbolLeaf(literal: ast.Boolean) =>
      et.Literal(literal)

    // Additionally treat #!unit as self-evaluating
    case sst.NonSymbolLeaf(literal: ast.Unit) =>
      et.Literal(literal)

    case malformed =>
      throw new MalformedExprException(malformed, malformed.toString)
  }).assignLocationAndContextFrom(datum, context.debugContext)
}
