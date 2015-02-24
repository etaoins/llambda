package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler._
import llambda.compiler.frontend.syntax.ParseSyntaxDefine

abstract sealed class ParsedDefine

/** Parsed single variable definition
  *
  * This is typically the result of parsing a normal (define)
  *
  * @param  definedSymbol          Symbol being defined
  * @param  providedType           Optional type annotation for the defined variable
  * @param  expr                   Closure providing the initialiser for the variable. This is a closure to allow the
  *                                scope to have any recursive bindings injected before evaluating the initialiser.
  * @param  storageLocConstructor  Closure constructing the storage location for the variable. This is used to implement
  *                                (define-report-procedure) which constructs specially annotated storage locations
  */
case class ParsedVarDefine(
    definedSymbol : sst.ScopedSymbol,
    providedType : Option[vt.SchemeType],
    expr : () => et.Expr,
    storageLocConstructor : (String, vt.SchemeType) => StorageLocation = (new StorageLocation(_, _))
) extends ParsedDefine

/** Target for a value in a multiple value define
  *
  * @param  definedSymbol  Symbol being defined
  * @param  providedType   Option type annotation for the defined variable
  */
case class ValueTarget(
    definedSymbol : sst.ScopedSymbol,
    providedType : Option[vt.SchemeType]
) {
  /** Creates a storage location for this value target in the defined symbol's scope */
  def createStorageLoc(defaultType : vt.SchemeType) : StorageLocation = {
    val schemeType = SchemeTypeForSymbol(definedSymbol, providedType, defaultType)
    val boundValue = new StorageLocation(definedSymbol.name, schemeType)

    definedSymbol.scope += (definedSymbol.name -> boundValue)

    boundValue
  }
}

/** Parsed multiple value define
  *
  * This is the result of parsing a (define-values)
  *
  * @param  fixedValueTargets   Targets for the fixed variables
  * @param  restValueTargetOpt  Optional target for the rest list variable
  * @param  expr                Closure providing the multiple value initialiser
  */
case class ParsedMultipleValueDefine(
    fixedValueTargets : List[ValueTarget],
    restValueTargetOpt : Option[ValueTarget],
    expr : () => et.Expr
) extends ParsedDefine

/** Parsed simple definition
  *
  * This directly associates a symbol with a bound value without requiring any initialiser expressions. This is used
  * for defining types and syntax
  *
  * @param  definedSymbol  Symbol being defined
  * @param  value          Bound value for the symbol
  */
case class ParsedSimpleDefine(definedSymbol : sst.ScopedSymbol, value : BoundValue) extends ParsedDefine

/** Parsed record type definition
  *
  * This is the result of parsing a (define-record-type)
  *
  * @param  typeSymbol  Symbol for the newly introduced record type
  * @param  recordType  The new record type
  * @param  procedures  The associated procedures for the record type
  */
case class ParsedRecordTypeDefine(
    typeSymbol : sst.ScopedSymbol,
    recordType : vt.RecordType,
    procedures : Map[sst.ScopedSymbol, et.ArtificialProcedure]
) extends ParsedDefine

/** Parsed type annotation
  *
  * The type annotation is injected directly in to the scope. This only exists as a signal that a definition was parsed
  */
case object ParsedTypeAnnotation extends ParsedDefine

object ParseDefine {
  private def parseMultipleValueDefine(
      argList : List[sst.ScopedDatum],
      argTerminator : sst.ScopedDatum,
      initialiserDatum : sst.ScopedDatum
  )(implicit context : FrontendContext) : ParsedMultipleValueDefine = {
    val parsedFormals = ParseFormals(argList, argTerminator)

    val fixedValueTargets = parsedFormals.fixedArgs map { case (symbol, schemeTypeOpt) =>
      ValueTarget(symbol, schemeTypeOpt)
    }

    val restValueTargetOpt = parsedFormals.restArgOpt map { case (symbol, memberTypeOpt) =>
      ValueTarget(symbol, memberTypeOpt.map(vt.UniformProperListType(_)))
    }

    ParsedMultipleValueDefine(
      fixedValueTargets=fixedValueTargets,
      restValueTargetOpt=restValueTargetOpt,
      expr=() => {ExtractInnerExpr(initialiserDatum) }
    )
  }

  /** Attempts to parse an application as a type of definition
    *
    * @param  located    Source location of the application
    * @param  boundVaue  Bound value being applied. If this corresponds to one of the definition primitives it will be parsed as
    *                    such
    * @param  operands   Operands the the application
    * @return Parsed definition or None if it cannot be parsed as a definition
    */
  def apply(
      located : SourceLocated,
      boundValue : BoundValue,
      operands : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : Option[ParsedDefine] =
    (boundValue, operands) match {
      case (Primitives.Define, List(symbol : sst.ScopedSymbol, value)) =>
        Some(ParsedVarDefine(symbol, None, () => {
          ExtractInnerExpr(value)
        }))

      case (Primitives.Define, List(
          symbol : sst.ScopedSymbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          value
      )) =>
        val providedType = ExtractType.extractStableType(typeDatum)(context.config)

        Some(ParsedVarDefine(symbol, Some(providedType), () => {
          ExtractInnerExpr(value)
        }))

      case (Primitives.Define, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        Some(ParsedVarDefine(symbol, None, () => {
          ExtractLambda(
            located=located,
            argList=fixedArgs,
            argTerminator=restArgDatum,
            definition=body,
            sourceNameHint=Some(symbol.name),
            typeDeclaration=LocTypeDeclarationForSymbol(symbol)
          ).assignLocationAndContextFrom(located, context.debugContext)
        }))

      case (Primitives.DefineValues, List(sst.ScopedListOrDatum(operands, operandTerminator), initialiser)) =>
        Some(parseMultipleValueDefine(operands, operandTerminator, initialiser))

      case (Primitives.DefineSyntax, _) =>
        Some(ParseSyntaxDefine(located, operands, context.debugContext))

      case (Primitives.DefineRecordType, _) =>
        Some(ParseRecordTypeDefine(located, operands)(context.config))

      case (Primitives.DefineType, (typeAlias : sst.ScopedSymbol) :: typeDatum :: Nil) =>
        // Allow the type's new name to be a recursion marker inside the definition
        val recursiveVars = ExtractType.RecursiveVars(Map(typeAlias.name -> 0))
        val extractedType = ExtractType.extractValueType(typeDatum, recursiveVars)

        Some(ParsedSimpleDefine(typeAlias, BoundType(extractedType)))

      case (Primitives.DefineType, sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: args) :: definition :: Nil) =>
        val typeConstructor = ExtractUserDefinedTypeConstructor(args, definition)
        Some(ParsedSimpleDefine(constructorName, typeConstructor))

      case (Primitives.DefineReportProcedure, List(symbol : sst.ScopedSymbol, definitionData)) =>
        Some(ParsedVarDefine(
          definedSymbol=symbol,
          providedType=None,
          expr=() => {
            ExtractInnerExpr(definitionData)
          },
          storageLocConstructor=(new ReportProcedure(_, _))
        ))

      case (Primitives.DefineReportProcedure, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        Some(ParsedVarDefine(
          definedSymbol=symbol,
          providedType=None,
          expr=() => {
            ExtractLambda(
              located=located,
              argList=fixedArgs,
              argTerminator=restArgDatum,
              definition=body,
              sourceNameHint=Some(symbol.name),
              typeDeclaration=LocTypeDeclarationForSymbol(symbol)
            ).assignLocationAndContextFrom(located, context.debugContext)
          },
          storageLocConstructor=(new ReportProcedure(_, _))
        ))

      case (Primitives.DefineNativeLibrary, List(libAlias : sst.ScopedSymbol, libDatum)) =>
        Some(ParsedSimpleDefine(libAlias, ExtractNativeLibrary(libDatum)))

      case (Primitives.AnnotateStorageLocType, List(declaredSymbol : sst.ScopedSymbol, typeDatum)) =>
        val declarationType = ExtractType.extractLocTypeDeclaration(typeDatum)

        declaredSymbol.scope.bindings.get(declaredSymbol.name) match {
          case None =>
            // No previous binding
          case Some(compatibleLoc : StorageLocation) if MonomorphicDeclaration(compatibleLoc.schemeType) == declarationType =>
            // Previous binding with compatible type
          case _ =>
            throw new BadSpecialFormException(declaredSymbol, "Symbol previously defined with incompatible value")
        }

        // Make sure there have been no incompatible declarations before
        LocTypeDeclarationForSymbol(declaredSymbol, Some(declarationType))

        // Record this declaration
        declaredSymbol.scope.typeDeclarations += (declaredSymbol -> declarationType)

        Some(ParsedTypeAnnotation)

      case _ => None
  }

}
