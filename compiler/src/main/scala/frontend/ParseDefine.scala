package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler._
import llambda.compiler.frontend.syntax.ParseSyntaxDefine

abstract sealed class ParsedDefine

/** Target for a value in a multiple value define
  *
  * @param  definedSymbol          Symbol being defined
  * @param  providedTypeOpt        Optional type annotation for the defined variable
  * @param  storageLocConstructor  Closure constructing the storage location for the variable. This is used to implement
  *                                (define-report-procedure) which constructs specially annotated storage locations
  */
case class ValueTarget(
    definedSymbol : sst.ScopedSymbol,
    providedTypeOpt : Option[vt.SchemeType],
    storageLocConstructor : (String, vt.SchemeType) => StorageLocation = (new StorageLocation(_, _))
) {
  /** Creates and binds a storage location for this value target in the defined symbol's scope */
  def bindStorageLoc(defaultType : vt.SchemeType) : StorageLocation = {
    val schemeType = SchemeTypeForSymbol(definedSymbol, providedTypeOpt, defaultType)
    val boundValue = storageLocConstructor(definedSymbol.name, schemeType)

    definedSymbol.scope += (definedSymbol.name -> boundValue)

    boundValue
  }
}

/** Parsed definition for zero or more variables
  *
  * @param  fixedValueTargets   Targets for the fixed variables
  * @param  restValueTargetOpt  Optional target for the rest list variable
  * @param  expr                Closure providing the multiple value initialiser
  */
case class ParsedVarsDefine(
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
  * @param  typeSymbol   Symbol for the newly introduced record type
  * @param  recordType   The new record type
  * @param  constructor  Constructor procedure for the record type
  * @param  procedures   The associated procedures for the record type
  */
case class ParsedRecordTypeDefine(
    typeSymbol : sst.ScopedSymbol,
    recordType : vt.RecordType,
    constructor : (sst.ScopedSymbol, et.RecordConstructor),
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
  )(implicit context : FrontendContext) : ParsedVarsDefine = {
    val parsedFormals = ParseFormals(argList, argTerminator)

    val fixedValueTargets = parsedFormals.fixedArgs map { case (symbol, schemeTypeOpt) =>
      ValueTarget(symbol, schemeTypeOpt)
    }

    val restValueTargetOpt = parsedFormals.restArgOpt map { case (symbol, memberTypeOpt) =>
      ValueTarget(symbol, memberTypeOpt.map(vt.UniformProperListType(_)))
    }

    ParsedVarsDefine(
      fixedValueTargets=fixedValueTargets,
      restValueTargetOpt=restValueTargetOpt,
      expr=() => {ExtractExpr(initialiserDatum) }
    )
  }

  /** Parses an application as a type of definition
    *
    * @param  located          Source location of the application
    * @param  primitiveDefine  Primitive define expression being applied
    * @param  operands         Operands the the application
    * @return Parsed definition or None if it cannot be parsed as a definition
    */
  def apply(
      located : SourceLocated,
      primitiveDefine : PrimitiveDefineExpr,
      operands : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : ParsedDefine =
    (primitiveDefine, operands) match {
      case (Primitives.Define, List(symbol : sst.ScopedSymbol, value)) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        ParsedVarsDefine(List(valueTarget), None, () => {
          ExtractExpr(value)
        })

      case (Primitives.Define, List(
          symbol : sst.ScopedSymbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          value
      )) =>
        val providedType = ExtractType.extractStableType(typeDatum)(context.config)
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=Some(providedType))

        ParsedVarsDefine(List(valueTarget), None, () => {
          ExtractExpr(value)
        })

      case (Primitives.Define, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        ParsedVarsDefine(List(valueTarget), None, () => {
          ExtractLambda(
            located=located,
            argList=fixedArgs,
            argTerminator=restArgDatum,
            definition=body,
            sourceNameHint=Some(symbol.name),
            typeDeclaration=LocTypeDeclarationForSymbol(symbol)
          ).assignLocationAndContextFrom(located, context.debugContext)
        })

      case (Primitives.DefineValues, List(sst.ScopedListOrDatum(operands, operandTerminator), initialiser)) =>
        parseMultipleValueDefine(operands, operandTerminator, initialiser)

      case (Primitives.DefineSyntax, _) =>
        ParseSyntaxDefine(located, operands, context.debugContext)

      case (Primitives.DefineRecordType, _) =>
        ParseRecordTypeDefine(located, operands)(context.config)

      case (Primitives.DefineType, (typeAlias : sst.ScopedSymbol) :: typeDatum :: Nil) =>
        // Allow the type's new name to be a recursion marker inside the definition
        val recursiveVars = ExtractType.RecursiveVars(Map(typeAlias.name -> 0))
        val extractedType = ExtractType.extractValueType(typeDatum, recursiveVars)

        ParsedSimpleDefine(typeAlias, BoundType(extractedType))

      case (Primitives.DefineType, sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: args) :: definition :: Nil) =>
        val typeConstructor = ExtractUserDefinedTypeConstructor(args, definition)
        ParsedSimpleDefine(constructorName, typeConstructor)

      case (Primitives.DefineReportProcedure, List(symbol : sst.ScopedSymbol, definitionData)) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new ReportProcedure(_, _))
        )

        ParsedVarsDefine(
          fixedValueTargets=List(valueTarget),
          restValueTargetOpt=None,
          expr=() => {
            ExtractExpr(definitionData)
          }
        )

      case (Primitives.DefineReportProcedure, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new ReportProcedure(_, _))
        )

        ParsedVarsDefine(
          fixedValueTargets=List(valueTarget),
          restValueTargetOpt=None,
          expr=() => {
            ExtractLambda(
              located=located,
              argList=fixedArgs,
              argTerminator=restArgDatum,
              definition=body,
              sourceNameHint=Some(symbol.name),
              typeDeclaration=LocTypeDeclarationForSymbol(symbol)
            ).assignLocationAndContextFrom(located, context.debugContext)
          }
        )

      case (Primitives.DefineNativeLibrary, List(libAlias : sst.ScopedSymbol, libDatum)) =>
        ParsedSimpleDefine(libAlias, ExtractNativeLibrary(libDatum))

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

        ParsedTypeAnnotation

      case _ =>
        throw new BadSpecialFormException(located, "Unrecognised definition syntax")
  }

}
