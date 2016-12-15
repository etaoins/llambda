package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler._

private[syntax] case class MatchConfig(
  ellipsisVariable: SyntaxVariable,
  literals: Set[SyntaxVariable]
) {
  val wildcardVariable = BoundSyntaxVariable(Primitives.Wildcard)

  /** Determines if the passed symbol should be treated as a zero or more match */
  def isZeroOrMore(scopedSymbol: sst.Symbol) =
    !literals.contains(ellipsisVariable) && (SyntaxVariable.fromSymbol(scopedSymbol) == ellipsisVariable)

  /** Determines if the passed symbol should be treated as a wildcard match */
  def isWildcard(scopedSymbol: sst.Symbol) =
    !literals.contains(wildcardVariable) && (SyntaxVariable.fromSymbol(scopedSymbol) == wildcardVariable)

  def withoutZeroOrMoreAllowed =
    this.copy(literals=this.literals + this.ellipsisVariable)


}

