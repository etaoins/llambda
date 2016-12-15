package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler._

object ExtractType {
  /** Represents all recursive variables known
    *
    * @param  variables  Map of a type variable to the depth of recursion it refers to
    */
  case class RecursiveVars(variables: Map[String, Int] = Map()) {
    /** Returns a copy of this instance with the depth its variables increased  */
    def recursed(recursionDepth: Int = 1): RecursiveVars = {
      val recursedVariables = variables.map({ case (name, depth) =>
        (name, depth + recursionDepth)
      })

      RecursiveVars(recursedVariables)
    }
  }

  private def resolveTypeConstructor(scopedSymbol: sst.Symbol): TypeConstructor = scopedSymbol.resolve match {
    case typeConstructor: TypeConstructor =>
      typeConstructor

    case Primitives.Quote =>
      LiteralTypeConstructor

    case _ =>
      throw new MalformedExprException(scopedSymbol, "Type constructor expected")
  }


  private def applyProcedureTypeConstructor(
      located: SourceLocated,
      args: List[sst.ScopedDatum]
  ): vt.ProcedureType = {
    // Explicitly recursive types cannot cross procedure boundaries due to lack of testing and use cases
    val noRecursiveVars = RecursiveVars()

    // Parse the constructor in to its components
    val parsed = ParseProcedureTypeConstructor(located, args)

    // Resolve the types
    val mandatoryArgTypes = parsed.fixedArgData.map(extractNonEmptySchemeType(_, noRecursiveVars))
    val restArgMemberType = parsed.restArgMemberDatumOpt.map(extractNonEmptySchemeType(_, noRecursiveVars))
    val returnType = extractReturnSchemeType(parsed.returnDatum)

    vt.ProcedureType(mandatoryArgTypes, Nil, restArgMemberType, returnType)
  }

  private def applyOptionalProcedureTypeConstructor(
      located: SourceLocated,
      args: List[sst.ScopedDatum]
  ): vt.ProcedureType = args match {
    case List(
      sst.ProperList(mandatoryArgData),
      sst.ProperList(optionalArgData),
      restArgMemberDatum,
      sst.Symbol(_, "*"),
      returnDatum
    ) =>
      val mandatoryArgTypes = mandatoryArgData.map(extractNonEmptySchemeType(_ , RecursiveVars()))
      val optionalArgTypes = optionalArgData.map(extractNonEmptySchemeType(_ , RecursiveVars()))
      val restArgMemberType = extractNonEmptySchemeType(restArgMemberDatum, RecursiveVars())
      val returnType = extractReturnSchemeType(returnDatum)

      vt.ProcedureType(mandatoryArgTypes, optionalArgTypes, Some(restArgMemberType), returnType)

    case List(sst.ProperList(mandatoryArgData), sst.ProperList(optionalArgData), returnDatum) =>
      val mandatoryArgTypes = mandatoryArgData.map(extractNonEmptySchemeType(_ , RecursiveVars()))
      val optionalArgTypes = optionalArgData.map(extractNonEmptySchemeType(_ , RecursiveVars()))
      val returnType = extractReturnSchemeType(returnDatum)

      vt.ProcedureType(mandatoryArgTypes, optionalArgTypes, None, returnType)

    case _ =>
      throw new BadSpecialFormException(located,
        "(->* (mandatory...) (optional...) return) or (->* (mandatory...) (optional...) rest * return) expected")
  }

  private def applyCaseProcedureTypeConstructor(located: SourceLocated, args: List[sst.ScopedDatum]): vt.SchemeType = {
    val locatedSignatures = args map { arg =>
      extractSchemeType(arg) match {
        case procType: vt.ProcedureType =>
          (arg, procType)

        case _ =>
          throw new BadSpecialFormException(arg, "case-> only accepts procedure type arguments")
      }
    }

    ValidateCaseLambdaClauses(locatedSignatures)
    vt.CaseProcedureType(locatedSignatures.map(_._2))
  }

  private def constructLiteralType(value: ast.Datum): vt.SchemeType = value match {
    case _: ast.EmptyList =>
      vt.EmptyListType

    case ast.Boolean(value) =>
      vt.LiteralBooleanType(value)

    case ast.Symbol(name) =>
      vt.LiteralSymbolType(name)

    case _ =>
      throw new BadSpecialFormException(value, s"Literal types for ${value.schemeType} are unsupported")
  }

  private def applyTypeConstructor(constructorName: sst.Symbol, args: List[sst.ScopedDatum], recursiveVars: RecursiveVars): vt.SchemeType = {
    resolveTypeConstructor(constructorName) match {
      case UserDefinedTypeConstructor(typeVars, definition) =>
        if (args.length != typeVars.length) {
          throw new BadSpecialFormException(constructorName, s"Type constructor expects ${typeVars.length} arguments, ${args.length} provided")
        }

        // Resolve the type the should be bound to each argument
        val argRecursiveVars = recursiveVars.recursed()
        val argTypes = typeVars.zip(args).map { case (typeVar, arg) =>
          (typeVar -> extractSchemeType(arg, argRecursiveVars))
        }

        // Reconcile the type vars with their upper bounds
        val reconciledVars = pm.ReconcileTypeVars(typeVars.toSet, pm.ResolveTypeVars.Result(argTypes.toMap), constructorName, true)

        // Instantiate the type
        pm.InstantiateType(reconciledVars, definition)

      case Primitives.UnionType =>
        val memberRecursiveVars = recursiveVars.recursed()

        val schemeTypeArgs = args.map(extractSchemeType(_, memberRecursiveVars))
        vt.SchemeType.fromTypeUnion(schemeTypeArgs)

      case Primitives.PairofType =>
        // Don't increase the depth of our recursive vars - the car/cdr references refer to the pair itself
        args.map(extractSchemeTypeRef(_, recursiveVars)) match {
          case List(carTypeRef, cdrTypeRef) =>
            vt.PairType(carTypeRef, cdrTypeRef)

          case _ =>
            throw new BadSpecialFormException(constructorName, "Pair constructor requires exactly two arguments")
        }

      case Primitives.ListofType =>
        args match {
          case List(memberDatum) =>
            vt.UnionType(Set(
              vt.EmptyListType,
              vt.SpecificPairType(
                carTypeRef=extractSchemeTypeRef(memberDatum, recursiveVars.recursed(1)),
                cdrTypeRef=vt.RecursiveSchemeTypeRef(1)
              )
            ))

          case _ =>
            throw new BadSpecialFormException(constructorName, "Listof requires exactly one member type argument")
        }

      case Primitives.ListType =>
        args.zipWithIndex.foldRight(vt.EmptyListType: vt.NonUnionSchemeType) {
          case ((arg, index), cdrType) =>
            val memberTypeRef = extractSchemeTypeRef(arg, recursiveVars.recursed(index))
            vt.SpecificPairType(memberTypeRef, cdrType)
        }

      case Primitives.RecType =>
        args match {
          case List(sst.Symbol(_, varName), innerTypeDatum) =>
            val newRecursiveVars = recursiveVars.copy(
              variables=recursiveVars.variables + (varName -> 0)
            )

            extractSchemeType(innerTypeDatum, newRecursiveVars)

          case _ =>
            throw new BadSpecialFormException(constructorName, "Rec requires a type variable and inner type as arguments")
        }

      case Primitives.ProcedureType =>
        applyProcedureTypeConstructor(constructorName, args)

      case Primitives.OptionalProcedureType =>
        applyOptionalProcedureTypeConstructor(constructorName, args)

      case Primitives.CaseProcedureType =>
        applyCaseProcedureTypeConstructor(constructorName, args)

      case Primitives.ExternalRecordType =>
        val sourceNameOpt = recursiveVars.variables.find(_._2 == 0).map(_._1)
        ExtractExternalRecordType(constructorName, sourceNameOpt, args)

      case Primitives.HashMapType =>
        val noRecursiveVars = RecursiveVars()

        args match {
          case List(keyTypeDatum, valueTypeDatum) =>
            val keyType = extractSchemeType(keyTypeDatum, noRecursiveVars)
            val valueType = extractSchemeType(valueTypeDatum, noRecursiveVars)

            vt.HashMapType(keyType, valueType)

          case _ =>
            throw new BadSpecialFormException(constructorName, "HashMap requires a key type and value type as arguments")
        }

      case LiteralTypeConstructor =>
        args match {
          case List(literalValue) =>
            constructLiteralType(literalValue.unscope)

          case _ =>
            throw new BadSpecialFormException(constructorName, "Quote requires exactly one member type argument")
        }

      case _ =>
        throw new BadSpecialFormException(constructorName, "Invalid type constructor syntax")
    }
  }

  private def extractSchemeTypeRef(
      datum: sst.ScopedDatum,
      recursiveVars: RecursiveVars
  ): vt.SchemeTypeRef = datum match {
    case sst.Symbol(_, varName) if recursiveVars.variables.contains(varName) =>
      vt.RecursiveSchemeTypeRef(recursiveVars.variables(varName))

    case nonRecursive =>
      // Increase the recursion level by 1 because we're going through a reference
      vt.DirectSchemeTypeRef(extractSchemeType(datum, recursiveVars.recursed()))
  }

  private def extractPolymorphicProcedure(
      located: SourceLocated,
      typeVarData: List[sst.ScopedDatum],
      args: List[sst.ScopedDatum],
      procedureTypeApplicator: (SourceLocated, List[sst.ScopedDatum]) => vt.ProcedureType
  ): pm.PolymorphicProcedureType = {
    val namedTypeVars = typeVarData map ExtractTypeVar

    // Rescope the definition
    val typeBindings = namedTypeVars map { case (name, typeVar) =>
      name -> (BoundType(typeVar): BoundValue)
    }

    val scopeMapping = Scope.mappingForBoundValues(typeBindings)

    val rescopedArgs = args.map(_.rescoped(scopeMapping))
    val template = procedureTypeApplicator(located, rescopedArgs)

    pm.PolymorphicProcedureType(namedTypeVars.map(_._2).toSet, template)
  }

  def extractSchemeType(
      datum: sst.ScopedDatum,
      recursiveVars: RecursiveVars = RecursiveVars()
  ): vt.SchemeType = extractValueType(datum, recursiveVars) match {
    case schemeType: vt.SchemeType =>
      schemeType

    case nonCellValue =>
      throw new BadSpecialFormException(datum, "Native type used where Scheme type expected")
  }

  def extractNonEmptySchemeType(
      datum: sst.ScopedDatum,
      recursiveVars: RecursiveVars = RecursiveVars()
  ): vt.SchemeType = extractSchemeType(datum, recursiveVars) match {
    case vt.EmptySchemeType =>
      throw new BadSpecialFormException(datum, "Empty Scheme type where non-empty type expected")

    case nonEmptyType =>
      nonEmptyType
  }

  def extractStableType(
      datum: sst.ScopedDatum,
      recursiveVars: RecursiveVars = RecursiveVars()
  )(implicit frontendConfig: FrontendConfig): vt.SchemeType = {
    val schemeType = extractSchemeType(datum, recursiveVars)
    val stableType = vt.StabiliseType(schemeType)

    if (schemeType != stableType) {
      throw new BadSpecialFormException(datum, s"Unstable type ${schemeType} used in context where it can be modified; closest stable type is ${stableType}")
    }
    else {
      schemeType
    }
  }

  def extractValueType(
      datum: sst.ScopedDatum,
      recursiveVars: RecursiveVars = RecursiveVars()
  ): vt.ValueType = datum match {
    case sst.Symbol(_, varName) if recursiveVars.variables.contains(varName) =>
      throw new BadSpecialFormException(datum, "Recursive type variable used where concrete type is expected")

    case symbol: sst.Symbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

        case typeConstructor: TypeConstructor =>
          throw new BadSpecialFormException(symbol, "Type constructor used as type")

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }

    case sst.ProperList((constructorName: sst.Symbol) :: argData) =>
      applyTypeConstructor(constructorName, argData, recursiveVars)

    case sst.NonSymbolLeaf(ast.Boolean(value)) =>
      vt.LiteralBooleanType(value)

    case nonsymbol =>
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol or type constructor application")
  }

  def extractReturnValueType(datum: sst.ScopedDatum): vt.ReturnType.ReturnType[vt.ValueType] =
    vt.ReturnType.Reachable(extractValueType(datum))

  def extractReturnSchemeType(datum: sst.ScopedDatum): vt.ReturnType.ReturnType[vt.SchemeType] =
    extractReturnValueType(datum) match {
      case vt.ReturnType.Unreachable =>
        vt.ReturnType.Unreachable

      case vt.ReturnType.Reachable(schemeType: vt.SchemeType) =>
        vt.ReturnType.Reachable(schemeType)

      case vt.ReturnType.Reachable(_) =>
        throw new BadSpecialFormException(datum, "Native return type used where Scheme type expected")
    }

  def extractLocTypeDeclaration(datum: sst.ScopedDatum): LocTypeDeclaration = datum match {
    case sst.ProperList(List(
      sst.ResolvedSymbol(Primitives.PolymorphicType),
      sst.ProperList(typeVarData),
      sst.ProperList(sst.ResolvedSymbol(Primitives.ProcedureType) :: args)
    )) =>
      val polyProcType = extractPolymorphicProcedure(datum, typeVarData, args, applyProcedureTypeConstructor)
      PolymorphicProcedureDeclaration(polyProcType)

    case sst.ProperList(List(
      sst.ResolvedSymbol(Primitives.PolymorphicType),
      sst.ProperList(typeVarData),
      sst.ProperList(sst.ResolvedSymbol(Primitives.OptionalProcedureType) :: args)
    )) =>
      val polyProcType = extractPolymorphicProcedure(datum, typeVarData, args, applyOptionalProcedureTypeConstructor)
      PolymorphicProcedureDeclaration(polyProcType)

    case _ =>
      MonomorphicDeclaration(extractSchemeType(datum))
  }
}
