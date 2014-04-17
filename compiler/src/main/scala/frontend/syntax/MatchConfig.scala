package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler._

private[syntax] case class MatchConfig(
  ellipsisIdentifier : String,
  literals : Set[SyntaxVariable]
) {
  /** Indicates if "..." matches should be allowed */
  val zeroOrMoreAllowed = 
    !literals.contains(UnboundSyntaxVariable(ellipsisIdentifier))

  /** Indicates if _ matches should be allowed */
  val wildcardAllowed = 
    !literals.contains(UnboundSyntaxVariable("_"))

  def withoutZeroOrMoreAllowed =
    this.copy(literals=this.literals + UnboundSyntaxVariable(this.ellipsisIdentifier))
}

