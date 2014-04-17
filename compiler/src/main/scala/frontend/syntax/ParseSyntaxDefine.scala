package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler.sst
import llambda.compiler._
import llambda.compiler.frontend.ParsedSimpleDefine

private[frontend] object ParseSyntaxDefine {
  private def checkDuplicateVariables(foundVariables : PatternVariables, seenVariables : Set[SyntaxVariable]) : Set[SyntaxVariable] = {
    val afterVisitedSelf = foundVariables.variables.foldLeft(seenVariables) { case (seenVariables, foundVariable) =>
      if (seenVariables.contains(foundVariable)) {
        throw new BadSpecialFormException(foundVariable, "Duplicate pattern variable")
      }

      seenVariables + foundVariable
    }

    foundVariables.subpatterns.foldLeft(afterVisitedSelf) { case (seenVariables, subpattern) => 
      checkDuplicateVariables(subpattern, seenVariables)
    }
  }

  private def parseTransformers(definedSymbol : sst.ScopedSymbol, ellipsisIdentifier : String, literalData : List[sst.ScopedDatum], rulesData : List[sst.ScopedDatum]) : ParsedSimpleDefine = {
    val literals = (literalData.map { 
      case symbol @ sst.ScopedSymbol(_, identifier) => 
        SyntaxVariable.fromSymbol(symbol)

      case nonSymbol =>
        throw new BadSpecialFormException(nonSymbol, "Symbol expected in literal list")
    }).toSet
    
    val parsedRules = rulesData map {
      case sst.ScopedProperList(sst.ScopedPair(_, patternDatum) :: template :: Nil) =>
        // Find all of our pattern variables
        val matchConfig = MatchConfig(
          ellipsisIdentifier=ellipsisIdentifier,
          literals=literals
        )

        val patternVariables = FindPatternVariables(patternDatum)(matchConfig)
          
        checkDuplicateVariables(patternVariables, Set()) 

        Transformer(patternDatum, patternVariables, template)

      case noMatch => throw new BadSpecialFormException(noMatch, "Unable to parse syntax rule")
    }

    ParsedSimpleDefine(definedSymbol, new BoundSyntax(ellipsisIdentifier, literals.toSet, parsedRules))
  }

  def apply(appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : ParsedSimpleDefine = operands match {
    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literalData) :: rulesData
             )) =>
      parseTransformers(definedSymbol, "...", literalData, rulesData)
    
    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedSymbol(_, ellipsisIdentifier) :: sst.ScopedProperList(literalData) :: rulesData
             )) =>
      parseTransformers(definedSymbol, ellipsisIdentifier, literalData, rulesData)

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized (define-syntax) form")
  }
}
