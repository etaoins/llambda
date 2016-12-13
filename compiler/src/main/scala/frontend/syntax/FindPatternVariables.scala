package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler._

private[syntax] object FindPatternVariables {
  private def vectorElementsPatternVariables(elements: List[sst.ScopedDatum])(implicit matchConfig: MatchConfig): PatternVariables = {
    elements match {
      case subpatternDatum :: (ellipsisSymbol: sst.ScopedSymbol) :: tailPattern
          if matchConfig.isZeroOrMore(ellipsisSymbol) =>
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

  def apply(pattern: sst.ScopedDatum)(implicit matchConfig: MatchConfig): PatternVariables = pattern match {
    case symbol: sst.ScopedSymbol  =>
      val patternVariable = SyntaxVariable.fromSymbol(symbol)

      if (matchConfig.literals.contains(patternVariable)) {
        // This is a literal
        PatternVariables()
      }
      else if (patternVariable == matchConfig.wildcardVariable) {
        // This is a wildcard
        PatternVariables()
      }
      else {
        // Found one!
        PatternVariables(
          variables=List(patternVariable)
        )
      }

    case sst.ScopedPair(subpatternDatum, sst.ScopedPair(ellipsisSymbol: sst.ScopedSymbol, cdr))
        if matchConfig.isZeroOrMore(ellipsisSymbol) =>
      // Ignore the ... and increase our depth
      val subpattern = apply(subpatternDatum)

      PatternVariables(
        subpatterns=Vector(subpattern)
      )

    case sst.ScopedPair(car, cdr) =>
      apply(car) ++ apply(cdr)

    case sst.ScopedVectorLiteral(innerPattern) =>
      vectorElementsPatternVariables(innerPattern.toList)

    case _: sst.NonSymbolLeaf =>
      // Can't contain symbols
      PatternVariables()
  }
}
