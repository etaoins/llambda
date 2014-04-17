package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler.sst
import llambda.compiler._

private[syntax] object FindPatternVariables {
  private def vectorElementsPatternVariables(elements : List[sst.ScopedDatum])(implicit matchConfig : MatchConfig) : PatternVariables = {
    elements match {
      case subpatternDatum :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: tailPattern if matchConfig.zeroOrMoreAllowed =>
        val subpattern = apply(subpatternDatum)

        PatternVariables(
          subpatterns=Vector(subpattern)
        ) 

      case patternDatum :: tailPattern =>
        apply(patternDatum) ++
          vectorElementsPatternVariables(tailPattern)

      case Nil =>
        PatternVariables()
    }
  }

  def apply(pattern : sst.ScopedDatum)(implicit matchConfig : MatchConfig) : PatternVariables = pattern match {
    case symbol : sst.ScopedSymbol  =>
      val patternVariable = SyntaxVariable.fromSymbol(symbol)

      if (matchConfig.literals.contains(patternVariable)) {
        // This is a literal
        PatternVariables()
      }
      else if (patternVariable == UnboundSyntaxVariable("_")) {
        // This is a wildcard
        PatternVariables()
      }
      else {
        // Found one!
        PatternVariables(
          variables=List(patternVariable)
        )
      }

    case sst.ScopedPair(subpatternDatum, sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), cdr)) if matchConfig.zeroOrMoreAllowed =>
      // Ignore the ... and increase our depth
      val subpattern = apply(subpatternDatum)

      PatternVariables(
        subpatterns=Vector(subpattern)
      ) 
    
    case sst.ScopedPair(car, cdr) =>
      apply(car) ++ apply(cdr)
    
    case sst.ScopedVectorLiteral(innerPattern) =>
      vectorElementsPatternVariables(innerPattern.toList)
    
    case _ : sst.NonSymbolLeaf =>
      // Can't contain symbols
      PatternVariables()
  }
}
