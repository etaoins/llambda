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
case class ExtractedVarsDefine(
    fixedValueTargets : List[ValueTarget],
    restValueTargetOpt : Option[ValueTarget],
    expr : () => et.Expr
)

object ExtractDefine {
  private def parseMultipleValueDefine(
      argList : List[sst.ScopedDatum],
      argTerminator : sst.ScopedDatum,
      initialiserDatum : sst.ScopedDatum
  )(implicit context : FrontendContext) : ExtractedVarsDefine = {
    val parsedFormals = ParseFormals(argList, argTerminator, allowOptionals=false)

    val fixedValueTargets = parsedFormals.mandatoryArgs map { case (symbol, schemeTypeOpt) =>
      ValueTarget(symbol, schemeTypeOpt)
    }

    val restValueTargetOpt = parsedFormals.restArgOpt map { case (symbol, memberTypeOpt) =>
      ValueTarget(symbol, memberTypeOpt.map(vt.UniformProperListType(_)))
    }

    ExtractedVarsDefine(
      fixedValueTargets=fixedValueTargets,
      restValueTargetOpt=restValueTargetOpt,
      expr=() => {ExtractExpr(initialiserDatum) }
    )
  }

  private def bindValue(symbol : sst.ScopedSymbol, value : BoundValue, allowRedefinition : Boolean) : Unit = {
    if (!allowRedefinition && symbol.scope.bindings.contains(symbol.name)) {
      throw new DuplicateDefinitionException(symbol)
    }

    symbol.scope += symbol.name -> value
  }

  /** Extracts an application as a type of definition
    *
    * @param  located            Source location of the application
    * @param  primitiveDefine    Primitive define expression being applied
    * @param  operands           Operands the the application
    * @param  allowRedefinition  Indicates if variables can be redefined within the same scope
    * @return Extracted definition or None if it cannot be parsed as a definition
    */
  def apply(
      located : SourceLocated,
      primitiveDefine : PrimitiveDefineExpr,
      operands : List[sst.ScopedDatum],
      allowRedefinition : Boolean
  )(implicit context : FrontendContext) : List[ExtractedVarsDefine] =
    (primitiveDefine, operands) match {
      case (Primitives.Define, List(symbol : sst.ScopedSymbol, value)) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        List(ExtractedVarsDefine(List(valueTarget), None, () => {
          ExtractExpr(value)
        }))

      case (Primitives.Define, List(
          symbol : sst.ScopedSymbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          value
      )) =>
        val providedType = ExtractType.extractStableType(typeDatum)(context.config)
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=Some(providedType))

        List(ExtractedVarsDefine(List(valueTarget), None, () => {
          ExtractExpr(value)
        }))

      case (Primitives.Define, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(definedSymbol=symbol, providedTypeOpt=None)

        List(ExtractedVarsDefine(List(valueTarget), None, () => {
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
        List(parseMultipleValueDefine(operands, operandTerminator, initialiser))

      case (Primitives.DefineSyntax, _) =>
        val (symbol, parsedSyntax) = ParseSyntaxDefine(located, operands, context.debugContext)
        bindValue(symbol, parsedSyntax, allowRedefinition)

        Nil

      case (Primitives.DefineRecordType, _) =>
        ParseRecordTypeDefine(located, operands)(context.config) match {
          case ParseRecordTypeDefine.Result(typeSymbol, recordType, (constructorSym, constructorExpr), procedures) =>
            bindValue(typeSymbol, BoundType(recordType), allowRedefinition)

            val constructorTarget = ValueTarget(
              definedSymbol=constructorSym,
              providedTypeOpt=None,
              storageLocConstructor=(new BoundRecordConstructor(constructorExpr, _, _))
            )

            val constructorDefine = ExtractedVarsDefine(List(constructorTarget), None, { () => constructorExpr })

            constructorDefine :: procedures.toList.map { case (procedureSymbol, expr) =>
              val schemeType = SchemeTypeForSymbol(procedureSymbol)

              val procTarget = ValueTarget(procedureSymbol, Some(schemeType))
              ExtractedVarsDefine(List(procTarget), None, { () => expr })
            }
        }

      case (Primitives.DefineType, (typeAlias : sst.ScopedSymbol) :: typeDatum :: Nil) =>
        // Allow the type's new name to be a recursion marker inside the definition
        val recursiveVars = ExtractType.RecursiveVars(Map(typeAlias.name -> 0))
        val extractedType = ExtractType.extractValueType(typeDatum, recursiveVars)

        bindValue(typeAlias, BoundType(extractedType), allowRedefinition)
        Nil

      case (Primitives.DefineType, sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: args) :: definition :: Nil) =>
        val typeConstructor = ExtractUserDefinedTypeConstructor(args, definition)
        bindValue(constructorName, typeConstructor, allowRedefinition)
        Nil

      case (Primitives.DefineReportProcedure, List(symbol : sst.ScopedSymbol, definitionData)) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new ReportProcedure(_, _))
        )

        List(ExtractedVarsDefine(
          fixedValueTargets=List(valueTarget),
          restValueTargetOpt=None,
          expr=() => {
            ExtractExpr(definitionData)
          }
        ))

      case (Primitives.DefineReportProcedure, sst.ScopedAnyList((symbol : sst.ScopedSymbol) :: fixedArgs, restArgDatum) :: body) =>
        val valueTarget = ValueTarget(
          definedSymbol=symbol,
          providedTypeOpt=None,
          storageLocConstructor=(new ReportProcedure(_, _))
        )

        List(ExtractedVarsDefine(
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
        ))

      case (Primitives.DefineNativeLibrary, List(libAlias : sst.ScopedSymbol, libDatum)) =>
        bindValue(libAlias, ExtractNativeLibrary(libDatum), allowRedefinition)
        Nil

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

        Nil

      case _ =>
        throw new BadSpecialFormException(located, "Unrecognised definition syntax")
  }

}
