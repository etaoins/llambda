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

  private def resolveTypeConstructor(scopedSymbol : sst.ScopedSymbol) : PrimitiveTypeConstructor = scopedSymbol.resolve match {
    case typeConstructor : PrimitiveTypeConstructor =>
      typeConstructor

    case _ =>
      throw new MalformedExprException(scopedSymbol, "Syntax cannot be used as an expression")
  }

  private def applyTypeConstructor(constructorName : sst.ScopedSymbol, operands : List[sst.ScopedDatum], recursiveVars : RecursiveVars) : vt.SchemeType = {
    resolveTypeConstructor(constructorName) match {
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
            vt.ProperListType(extractSchemeTypeRef(memberDatum, memberRecursiveVars))

          case _ =>
            throw new BadSpecialFormException(constructorName, "Listof requires exactly one member type argument")
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

  def extractValueType(
      datum : sst.ScopedDatum,
      recursiveVars : RecursiveVars = RecursiveVars()
  ) : vt.ValueType = datum match { 
    case sst.ScopedSymbol(_, varName) if recursiveVars.variables.contains(varName) =>
      throw new BadSpecialFormException(datum, "Recursive type variable used where concrete type is expected")

    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

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
