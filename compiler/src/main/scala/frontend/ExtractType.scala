package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{valuetype => vt}
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

    case _ =>
      throw new MalformedExprException(scopedSymbol, "Type constructor expected")
  }

  private def applyTypeConstructor(constructorName : sst.ScopedSymbol, operands : List[sst.ScopedDatum], recursiveVars : RecursiveVars) : vt.SchemeType = {
    resolveTypeConstructor(constructorName) match {
      case UserDefinedTypeConstructor(constructorArgs, definition) =>
        if (operands.length != constructorArgs.length) {
          throw new BadSpecialFormException(constructorName, s"Type constructor expects ${constructorArgs.length} arguments, ${operands.length} provided")
        }

        // Resolve the type the should be bound to each argument
        val operandRecursiveVars = recursiveVars.recursed()
        val argTypes = constructorArgs.zip(operands).map { case (constructorArgSymbol, operand) =>
          (constructorArgSymbol -> extractValueType(operand, operandRecursiveVars))
        }

        // Create new scopes that bind the arguments to their new types
        val argsForScope = argTypes groupBy(_._1.scope)

        val scopeMapping = argsForScope map { case (oldScope, scopeArgTypes) =>
          val bindings = collection.mutable.Map(scopeArgTypes.map { case (constructorArgSymbol, valueType) =>
            constructorArgSymbol.name -> (BoundType(valueType) : BoundValue)
          } : _*)

          (oldScope -> new Scope(bindings, Some(oldScope)))
        }

        // Process the rescoped definition
        extractSchemeType(definition.rescoped(scopeMapping), recursiveVars)

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
        val memberRecursiveVars = recursiveVars.recursed()

        operands match {
          case List(memberDatum) =>
            vt.UniformProperListType(extractSchemeTypeRef(memberDatum, memberRecursiveVars))

          case _ =>
            throw new BadSpecialFormException(constructorName, "Listof requires exactly one member type argument")
        }

      case Primitives.ListType =>
        val memberTypeRefs = operands.zipWithIndex map { case (operand, index) =>
          extractSchemeTypeRef(operand, recursiveVars.recursed(index))
        }

        vt.SpecificProperListType(memberTypeRefs)
      
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

  def extractSchemeType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  ) : vt.SchemeType = extractValueType(datum, recursiveVars) match {
    case schemeType : vt.SchemeType =>
      schemeType

    case nonCellValue =>
      throw new BadSpecialFormException(datum, "Native type used where Scheme type expected")
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
        case BoundType(schemeType) => schemeType

        case typeConstructor : TypeConstructor =>
          throw new BadSpecialFormException(symbol, "Type constructor used as type")

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }
    
    case sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: operandData) =>
      applyTypeConstructor(constructorName, operandData, recursiveVars)

    case sst.NonSymbolLeaf(ast.BooleanLiteral(value)) =>
      vt.ConstantBooleanType(value)

    case nonsymbol => 
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol or type constructor application")
  }
}
