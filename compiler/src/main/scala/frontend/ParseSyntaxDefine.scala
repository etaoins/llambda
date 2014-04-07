package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler._

private[frontend] object ParseSyntaxDefine {
  def findPatternVariables(ellipsisIdentifier : String, literals : Set[SyntaxVariable], pattern : List[sst.ScopedDatum], depth : Int) : List[MacroPatternVariable] = {
    val zeroOrMoreAllowed = !literals.contains(UnboundSyntaxVariable(ellipsisIdentifier))

    pattern match {
      case (subpatternDatum :: sst.ScopedSymbol(_, `ellipsisIdentifier`) :: patternTail) if zeroOrMoreAllowed =>
        // Ignore the ... and increase our depth
        findPatternVariables(ellipsisIdentifier, literals, List(subpatternDatum), depth + 1) ++
          findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)

      case sst.ScopedProperList(innerPattern) :: patternTail =>
        findPatternVariables(ellipsisIdentifier, literals, innerPattern, depth) ++
          findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)
      
      case sst.ScopedImproperList(innerPattern, termPattern) :: patternTail =>
        findPatternVariables(ellipsisIdentifier, literals, innerPattern, depth) ++
          findPatternVariables(ellipsisIdentifier, literals, List(termPattern), depth) ++
          findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)
      
      case sst.ScopedVectorLiteral(innerPattern) :: patternTail =>
        findPatternVariables(ellipsisIdentifier, literals, innerPattern.toList, depth) ++
          findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)
      
      case (symbol : sst.ScopedSymbol) :: patternTail  =>
        val patternVariable = SyntaxVariable.fromSymbol(symbol)

        val foundVariableOpt = if (literals.contains(patternVariable)) {
          // This is a literal
          None
        }
        else if (patternVariable == UnboundSyntaxVariable("_")) {
          // This is a wildcard
          None
        }
        else {
          // Found one!
          Some(MacroPatternVariable(depth, patternVariable).assignLocationFrom(symbol))
        }

        foundVariableOpt.toList ++ findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)

      case (_ : sst.NonSymbolLeaf) :: patternTail =>
        // Can't contain symbols
        findPatternVariables(ellipsisIdentifier, literals, patternTail, depth)

      case Nil => 
        // At the end
        Nil
    }
  }

  private def parseSyntaxRules(definedSymbol : sst.ScopedSymbol, ellipsisIdentifier : String, literalData : List[sst.ScopedDatum], rulesData : List[sst.ScopedDatum]) : ParsedSimpleDefine = {
    val literals = (literalData.map { 
      case symbol @ sst.ScopedSymbol(_, identifier) => 
        SyntaxVariable.fromSymbol(symbol)

      case nonSymbol =>
        throw new BadSpecialFormException(nonSymbol, "Symbol expected in literal list")
    }).toSet
    
    val parsedRules = rulesData map {
      case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
        // Find all of our pattern variables
        val foundVariables = findPatternVariables(ellipsisIdentifier, literals, pattern, 0)
          
        // Check for duplicate variables
        // Do this before expansion time both to error our early and to simplify the expansion code slightly
        foundVariables.foldLeft(Set[SyntaxVariable]()) { case (seenVariables, foundVariable) =>
          if (seenVariables.contains(foundVariable.variable)) {
            throw new BadSpecialFormException(foundVariable, "Duplicate pattern variable")
          }

          seenVariables + foundVariable.variable
        }

        SyntaxRule(pattern, template)

      case noMatch => throw new BadSpecialFormException(noMatch, "Unable to parse syntax rule")
    }

    ParsedSimpleDefine(definedSymbol, new BoundSyntax(ellipsisIdentifier, literals.toSet, parsedRules))
  }

  def apply(appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : ParsedSimpleDefine = operands match {
    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literalData) :: rulesData
             )) =>
      parseSyntaxRules(definedSymbol, "...", literalData, rulesData)
    
    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedSymbol(_, ellipsisIdentifier) :: sst.ScopedProperList(literalData) :: rulesData
             )) =>
      parseSyntaxRules(definedSymbol, ellipsisIdentifier, literalData, rulesData)

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized (define-syntax) form")
  }
}
