package llambda

class NoSyntaxRuleException(message : String) extends SemanticException(message)
class AmbiguousSyntaxRuleException(message : String) extends SemanticException(message)

object ExpandMacro {
  case class Rewrite(scope : Scope, identifier : String, expansion : sst.ScopedDatum)
  case class Expandable(template : sst.ScopedDatum, rewrites : List[Rewrite])

  private def matchRule(literals : List[String], pattern : List[sst.ScopedDatum], operands : List[sst.ScopedDatum]) : Option[List[Rewrite]] = {
    (pattern, operands) match {
      case (sst.ScopedSymbol(_, "_") :: restPattern, _ :: restOperands) =>
        // They used a wildcard - ignore this
        matchRule(literals, restPattern, restOperands)

      case (sst.ScopedSymbol(_, patternIdent) :: restPattern, sst.ScopedSymbol(_, operandIdent) :: restOperands) if literals.contains(patternIdent) =>
        if (patternIdent != operandIdent) {
          // They misused a literal - no match
          None
        }

        // The literal doesn't cause any rewrites. We just ignore it
        matchRule(literals, restOperands, restOperands)

      case (sst.ScopedSymbol(patternScope, patternIdent) :: restPattern, operand :: restOperands) =>
        // If the match doesn't fail later add our rewrite rule on
        matchRule(literals, restPattern, restOperands) map { rewrites =>
          Rewrite(patternScope, patternIdent, operand) :: rewrites
        }

      case (Nil, Nil) =>
        // We both ended at the same time - this is expected
        Some(Nil)

      case _ =>
        None
    }
  }

  def expandTemplate(template : sst.ScopedDatum, rewrites : List[Rewrite]) : sst.ScopedDatum = {
    for(rewrite <- rewrites) {
      if (template == sst.ScopedSymbol(rewrite.scope, rewrite.identifier)) {
        // The expansion should be already fully expanded - we don't need to recurse
        return rewrite.expansion
      }
    }

    template match {
      case sst.ScopedPair(scope, car, cdr) =>
        sst.ScopedPair(scope, expandTemplate(car, rewrites), expandTemplate(cdr, rewrites))

      case _ =>
        template
    }
  }

  def apply(syntax : BoundSyntax, operands : List[sst.ScopedDatum]) : sst.ScopedDatum = {
    val possibleExpandables = syntax.rules.flatMap { rule =>
      matchRule(syntax.literals, rule.pattern, operands) map { rewrites =>
        Expandable(rule.template, rewrites)
      }
    }

    possibleExpandables match {
      case Nil => throw new NoSyntaxRuleException(syntax.toString)
      case Expandable(template, rewrites) :: Nil =>
        // Expand!
        expandTemplate(template, rewrites)

      case _ =>
        throw new AmbiguousSyntaxRuleException(syntax.toString)
    }
  }
}

