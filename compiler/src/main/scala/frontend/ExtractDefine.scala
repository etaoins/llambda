package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler._
import llambda.compiler.frontend.syntax.ParseSyntaxDefine

/** Target for a value in a multiple value define
  *
  * @param  definedSymbol          Symbol being defined
  * @param  providedTypeOpt        Optional type annotation for the defined variable
  * @param  storageLocConstructor  Closure constructing the storage location for the variable. This is used to implement
  *                                (define-stdlib-procedure) which constructs specially annotated storage locations
  */
case class ValueTarget(
    definedSymbol: sst.Symbol,
    providedTypeOpt: Option[vt.SchemeType],
    storageLocConstructor: (String, vt.SchemeType) => StorageLocation = (new StorageLocation(_, _))
) {
  /** Creates and binds a storage location for this value target in the defined symbol's scope */
  def bindStorageLoc(defaultType: vt.SchemeType): StorageLocation = {
    val schemeType = SchemeTypeForSymbol(definedSymbol, providedTypeOpt, defaultType)
    val boundValue = storageLocConstructor(definedSymbol.name, schemeType)

    definedSymbol.scope += (definedSymbol.name -> boundValue)

    boundValue
  }
}

/** Parsed definition for a variable
  *
  * @param  valueTarget   Target for the initialiser's value
  * @param  expr          Closure providing the value initialiser
  */
case class ExtractedVarDefine(
    valueTarget: ValueTarget,
    expr: () => et.Expr
)

object ExtractDefine {
  private def bindValue(symbol: sst.Symbol, value: BoundValue): Unit = {
    if (symbol.scope.bindings.contains(symbol.name)) {
      throw new DuplicateDefinitionException(symbol)
    }

    symbol.scope += symbol.name -> value
  }

  /** Extracts an application as a type of definition
    *
    * @param  located            Source location of the application
    * @param  primitiveDefine    Primitive define expression being applied
    * @param  operands           Operands the the application
    * @return Extracted definition or None if it cannot be parsed as a definition
    */
  def apply(
      located: SourceLocated,
      primitiveDefine: PrimitiveDefineExpr,
      operands: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): List[ExtractedVarDefine] =
    (primitiveDefine, operands) match {
      case (Primitives.Define, List(symbol: sst.Symbol, value)) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        List(ExtractedVarDefine(valueTarget, () => {
          ExtractExpr(value)
        }))

      case (Primitives.Define, List(
          symbol: sst.Symbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          value
      )) =>
        val providedType = ExtractType.extractStableType(typeDatum)(context.config)
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=Some(providedType))

        List(ExtractedVarDefine(valueTarget, () => {
          ExtractExpr(value)
        }))

      case (Primitives.Define, sst.AnyList((symbol: sst.Symbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        List(ExtractedVarDefine(valueTarget, () => {
          ExtractLambda(
            located=located,
            argList=fixedArgs,
            argTerminator=restArgDatum,
            definition=body,
            sourceNameHint=Some(symbol.name),
            typeDeclaration=LocTypeDeclarationForSymbol(symbol)
          ).assignLocationAndContextFrom(located, context.debugContext)
        }))

      case (Primitives.DefineSyntax, _) =>
        val (symbol, parsedSyntax) = ParseSyntaxDefine(located, operands, context.debugContext)
        bindValue(symbol, parsedSyntax)

        Nil

      case (Primitives.DefineRecordType, _) =>
        ParseRecordTypeDefine(located, operands)(context.config) match {
          case ParseRecordTypeDefine.Result(typeSymbol, recordType, (constructorSym, constructorExpr), procedures) =>
            bindValue(typeSymbol, BoundType(recordType))

            val constructorTarget = ValueTarget(
              definedSymbol=constructorSym,
              providedTypeOpt=None,
              storageLocConstructor=(new BoundRecordConstructor(constructorExpr, _, _))
            )

            val constructorDefine = ExtractedVarDefine(constructorTarget, { () => constructorExpr })

            constructorDefine :: procedures.toList.map { case (procedureSymbol, expr) =>
              val schemeType = SchemeTypeForSymbol(procedureSymbol)

              val procTarget = ValueTarget(procedureSymbol, Some(schemeType))
              ExtractedVarDefine(procTarget, { () => expr })
            }
        }

      case (Primitives.DefineType, (typeAlias: sst.Symbol) :: typeDatum :: Nil) =>
        // Allow the type's new name to be a recursion marker inside the definition
        val recursiveVars = ExtractType.RecursiveVars(Map(typeAlias.name -> 0))
        val extractedType = ExtractType.extractValueType(typeDatum, recursiveVars)

        bindValue(typeAlias, BoundType(extractedType))
        Nil

      case (Primitives.DefineType, sst.ProperList((constructorName: sst.Symbol) :: args) :: definition :: Nil) =>
        val typeConstructor = ExtractUserDefinedTypeConstructor(args, definition)
        bindValue(constructorName, typeConstructor)
        Nil

      case (Primitives.DefineStdlibProcedure, List(symbol: sst.Symbol, definitionData)) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new StdlibProcedure(_, _))
        )

        List(ExtractedVarDefine(valueTarget, expr=() => {
            ExtractExpr(definitionData)
          }
        ))

      case (Primitives.DefineStdlibProcedure, sst.AnyList((symbol: sst.Symbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new StdlibProcedure(_, _))
        )

        List(ExtractedVarDefine(
          valueTarget,
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
        ))

      case (Primitives.DefineNativeLibrary, List(libAlias: sst.Symbol, libDatum)) =>
        bindValue(libAlias, ExtractNativeLibrary(libDatum))
        Nil

      case (Primitives.AnnotateStorageLocType, List(declaredSymbol: sst.Symbol, typeDatum)) =>
        val declarationType = ExtractType.extractLocTypeDeclaration(typeDatum)

        declaredSymbol.scope.bindings.get(declaredSymbol.name) match {
          case None =>
            // No previous binding
          case Some(compatibleLoc: StorageLocation) if MonomorphicDeclaration(compatibleLoc.schemeType) == declarationType =>
            // Previous binding with compatible type
          case _ =>
            throw new BadSpecialFormException(declaredSymbol, "Symbol previously defined with incompatible value")
        }

        // Make sure there have been no incompatible declarations before
        LocTypeDeclarationForSymbol(declaredSymbol, Some(declarationType))

        // Record this declaration
        declaredSymbol.scope.typeDeclarations += (declaredSymbol -> declarationType)

        Nil

      case _ =>
        throw new BadSpecialFormException(located, "Unrecognised definition syntax")
  }

}
