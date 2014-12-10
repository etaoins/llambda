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
  case class RecursiveVars(variables : Map[String, Int] = Map()) {
    /** Returns a copy of this instance with the depth its variables increased  */
    def recursed(recursionDepth : Int = 1) : RecursiveVars = {
      val recursedVariables = variables.map({ case (name, depth) =>
        (name, depth + recursionDepth)
      })

      RecursiveVars(recursedVariables)
    }
  }

  private def resolveTypeConstructor(scopedSymbol : sst.ScopedSymbol) : TypeConstructor = scopedSymbol.resolve match {
    case typeConstructor : TypeConstructor =>
      typeConstructor

    case Primitives.Quote =>
      LiteralTypeConstructor

    case _ =>
      throw new MalformedExprException(scopedSymbol, "Type constructor expected")
  }


  private def applyProcedureTypeConstructor(located : SourceLocated, operands : List[sst.ScopedDatum]) : vt.ProcedureType = {
    // Explicitly recursive types cannot cross procedure boundaries due to lack of testing and use cases
    val noRecursiveVars  = RecursiveVars()

    // Parse the constructor in to its components
    val parsed = ParseProcedureTypeConstructor(located, operands)

    // Resolve the types
    val fixedArgTypes = parsed.fixedArgData.map(extractNonEmptySchemeType(_, noRecursiveVars))
    val restArgMemberType = parsed.restArgMemberDatumOpt.map(extractNonEmptySchemeType(_, noRecursiveVars))
    val returnType = extractReturnSchemeType(parsed.returnDatum)

    vt.ProcedureType(fixedArgTypes, restArgMemberType, returnType)
  }

  private def applyCaseProcedureTypeConstructor(located : SourceLocated, operands : List[sst.ScopedDatum]) : vt.SchemeType = {
    val locatedSignatures = operands map { operand =>
      extractSchemeType(operand) match {
        case procType : vt.ProcedureType =>
          (operand, procType)

        case _ =>
          throw new BadSpecialFormException(operand, "case-> only accepts procedure type arguments")
      }
    }

    ValidateCaseLambdaClauses(locatedSignatures)
    vt.CaseProcedureType(locatedSignatures.map(_._2))
  }

  private def constructLiteralType(value : ast.Datum) : vt.SchemeType = value match {
    case _ : ast.EmptyList =>
      vt.EmptyListType

    case ast.BooleanLiteral(value) =>
      vt.LiteralBooleanType(value)

    case ast.Symbol(name) =>
      vt.LiteralSymbolType(name)

    case _ =>
      throw new BadSpecialFormException(value, s"Literal types for ${value.schemeType} are unsupported")
  }

  private def applyTypeConstructor(constructorName : sst.ScopedSymbol, operands : List[sst.ScopedDatum], recursiveVars : RecursiveVars) : vt.SchemeType = {
    resolveTypeConstructor(constructorName) match {
      case NonRecordTypeConstructor(typeVars, definition) =>
        if (operands.length != typeVars.length) {
          throw new BadSpecialFormException(constructorName, s"Type constructor expects ${typeVars.length} arguments, ${operands.length} provided")
        }

        // Resolve the type the should be bound to each argument
        val operandRecursiveVars = recursiveVars.recursed()
        val argTypes = typeVars.zip(operands).map { case (typeVar, operand) =>
          (typeVar -> extractSchemeType(operand, operandRecursiveVars))
        }

        // Reconcile the type vars with their upper bounds
        val reconciledVars = pm.ReconcileTypeVars(typeVars.toSet, pm.ResolveTypeVars.Result(argTypes.toMap), constructorName, true)

        // Instantiate the type
        pm.InstantiateType(reconciledVars, definition)

      case RecordTypeConstructor(recordType) =>
        val typeVars = recordType.typeVars

        if (operands.length != typeVars.length) {
          throw new BadSpecialFormException(constructorName, s"Record type constructor expects ${typeVars.length} arguments, ${operands.length} provided")
        }

        // Extract our type arguments
        val argTypes = typeVars.zip(operands).map { case (typeVar, operand) =>
          (typeVar -> extractSchemeType(operand))
        }

        val reconciledVars = pm.ReconcileTypeVars(typeVars.toSet, pm.ResolveTypeVars.Result(argTypes.toMap), constructorName, true)
        vt.RecordTypeInstance(reconciledVars, recordType)

      case Primitives.UnionType =>
        val memberRecursiveVars = recursiveVars.recursed()

        val schemeTypeOperands = operands.map(extractSchemeType(_, memberRecursiveVars))
        vt.SchemeType.fromTypeUnion(schemeTypeOperands)
      
      case Primitives.PairofType =>
        // Don't increase the depth of our recursive vars - the car/cdr references refer to the pair itself
        operands.map(extractSchemeTypeRef(_, recursiveVars)) match {
          case List(carTypeRef, cdrTypeRef) =>
            vt.PairType(carTypeRef, cdrTypeRef)

          case _ =>
            throw new BadSpecialFormException(constructorName, "Pair constructor requires exactly two arguments")
        }
      
      case Primitives.ListofType =>
        operands match {
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
        operands.zipWithIndex.foldRight(vt.EmptyListType : vt.NonUnionSchemeType) {
          case ((operand, index), cdrType) =>
            val memberTypeRef = extractSchemeTypeRef(operand, recursiveVars.recursed(index))
            vt.SpecificPairType(memberTypeRef, cdrType)
        }

      case Primitives.RecType =>
        operands match {
          case List(sst.ScopedSymbol(_, varName), innerTypeDatum) =>
            val newRecursiveVars = recursiveVars.copy(
              variables=recursiveVars.variables + (varName -> 0)
            )

            extractSchemeType(innerTypeDatum, newRecursiveVars)

          case _ =>
            throw new BadSpecialFormException(constructorName, "Rec requires a type variable and inner type as arguments")
        }

      case Primitives.VectorofType =>
        operands match {
          case List(memberDatum) =>
            vt.VectorOfType(extractSchemeTypeRef(memberDatum, recursiveVars))

          case _ =>
            throw new BadSpecialFormException(constructorName, "Vectorof requires exactly one member type argument")
        }
      
      case Primitives.VectorType =>
        val memberTypeRefs = operands.map { memberDatum =>
          extractSchemeTypeRef(memberDatum, recursiveVars)
        }

        vt.SpecificVectorType(memberTypeRefs.toVector)

      case Primitives.ProcedureType =>
        applyProcedureTypeConstructor(constructorName, operands)
      
      case Primitives.CaseProcedureType =>
        applyCaseProcedureTypeConstructor(constructorName, operands)

      case LiteralTypeConstructor =>
        operands match {
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
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars
  ) : vt.SchemeTypeRef = datum match {
    case sst.ScopedSymbol(_, varName) if recursiveVars.variables.contains(varName) =>
      vt.RecursiveSchemeTypeRef(recursiveVars.variables(varName))

    case nonRecursive =>
      // Increase the recursion level by 1 because we're going through a reference
      vt.DirectSchemeTypeRef(extractSchemeType(datum, recursiveVars.recursed()))
  }

  private def extractPolymorphicProcedure(
      located : SourceLocated,
      typeVarData : List[sst.ScopedDatum],
      operands : List[sst.ScopedDatum]
  ) : pm.PolymorphicProcedureType = {
    val namedTypeVars = typeVarData map ExtractTypeVar

    // Rescope the definition
    val typeBindings = namedTypeVars map { case (name, typeVar) =>
      name -> (BoundType(typeVar) : BoundValue)
    }

    val scopeMapping = Scope.mappingForBoundValues(typeBindings)

    val rescopedOperands = operands.map(_.rescoped(scopeMapping))
    val template = applyProcedureTypeConstructor(located, rescopedOperands)

    pm.PolymorphicProcedureType(namedTypeVars.map(_._2).toSet, template)
  }

  def extractSchemeType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  ) : vt.SchemeType = extractValueType(datum, recursiveVars) match {
    case schemeType : vt.SchemeType =>
      schemeType

    case nonCellValue =>
      throw new BadSpecialFormException(datum, "Native type used where Scheme type expected")
  }

  def extractNonEmptySchemeType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  ) : vt.SchemeType = extractSchemeType(datum, recursiveVars) match {
    case vt.EmptySchemeType =>
      throw new BadSpecialFormException(datum, "Empty Scheme type where non-empty type expected")

    case nonEmptyType =>
      nonEmptyType
  }

  def extractStableType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  )(implicit frontendConfig : FrontendConfig) : vt.SchemeType = {
    val schemeType = extractSchemeType(datum, recursiveVars)
    val stableType = vt.StabiliseType(schemeType, frontendConfig.schemeDialect)

    if (schemeType != stableType) {
      throw new BadSpecialFormException(datum, s"Unstable type ${schemeType} used in context where it can be modified; closest stable type is ${stableType}")
    }
    else {
      schemeType
    }
  }

  def extractValueType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  ) : vt.ValueType = datum match {
    case sst.ScopedSymbol(_, varName) if recursiveVars.variables.contains(varName) =>
      throw new BadSpecialFormException(datum, "Recursive type variable used where concrete type is expected")

    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(poison : pm.PoisonTypeVar) =>
          // Type variable used in an illegal position
          throw new BadSpecialFormException(datum, poison.message)

        case BoundType(schemeType) => schemeType

        case typeConstructor : TypeConstructor =>
          throw new BadSpecialFormException(symbol, "Type constructor used as type")

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }

    case sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: operandData) =>
      applyTypeConstructor(constructorName, operandData, recursiveVars)

    case sst.NonSymbolLeaf(ast.BooleanLiteral(value)) =>
      vt.LiteralBooleanType(value)

    case nonsymbol =>
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol or type constructor application")
  }

  def extractReturnValueType(datum : sst.ScopedDatum) : vt.ReturnType.ReturnType[vt.ValueType] = datum match {
    case sst.ScopedSymbol(_, "*") =>
      vt.ReturnType.ArbitraryValues

    case sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: operandData) =>
      constructorName.resolve match {
        case Primitives.ValuesType =>
          val valueTypes = operandData.map(extractNonEmptySchemeType(_))
          vt.ReturnType.SpecificValues(valueTypes)

        case _ =>
          vt.ReturnType.SingleValue(
            applyTypeConstructor(constructorName, operandData, RecursiveVars())
          )
      }

    case otherDatum =>
      vt.ReturnType.SingleValue(extractValueType(otherDatum))
  }

  def extractReturnSchemeType(datum : sst.ScopedDatum) : vt.ReturnType.ReturnType[vt.SchemeType] =
    extractReturnValueType(datum) match {
      case vt.ReturnType.SingleValue(schemeType : vt.SchemeType) =>
        vt.ReturnType.SingleValue(schemeType)

      case vt.ReturnType.SingleValue(_) =>
        throw new BadSpecialFormException(datum, "Native return type used where Scheme type expected")

      case multipleValues : vt.ReturnType.MultipleValues =>
        multipleValues
    }

  def extractLocTypeDeclaration(datum : sst.ScopedDatum) : LocTypeDeclaration = datum match {
    // Long form
    case sst.ScopedProperList(List(
      sst.ResolvedSymbol(Primitives.PolymorphicType),
      sst.ScopedProperList(typeVarData),
      sst.ScopedProperList(sst.ResolvedSymbol(Primitives.ProcedureType) :: operands)
    )) =>
      PolymorphicProcedureDeclaration(extractPolymorphicProcedure(datum, typeVarData, operands))

    // Shorthand
    case sst.ScopedProperList(
      sst.ResolvedSymbol(Primitives.PolymorphicType) ::
      sst.ScopedProperList(typeVarData) ::
      operands
    ) =>
      PolymorphicProcedureDeclaration(extractPolymorphicProcedure(datum, typeVarData, operands))

    case _ =>
      MonomorphicDeclaration(extractSchemeType(datum))
  }
}
