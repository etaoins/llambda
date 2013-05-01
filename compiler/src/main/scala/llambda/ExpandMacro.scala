package llambda

class NoSyntaxRuleException(message : String) extends SemanticException(message)
class AmbiguousSyntaxRuleException(message : String) extends SemanticException(message)

object ExpandMacro {
  sealed abstract class Rewrite
  case class SubstituteRewrite(scope : Scope, identifier : String, expansion : sst.ScopedDatum) extends Rewrite
  case class SpliceRewrite(scope : Scope, identifier : String, expansion : List[sst.ScopedDatum]) extends Rewrite

  case class Expandable(template : sst.ScopedDatum, rewrites : List[Rewrite])

  private def matchRule(literals : List[String], pattern : List[sst.ScopedDatum], operands : List[sst.ScopedDatum]) : Option[List[Rewrite]] = {
    (pattern, operands) match {
      case (sst.ScopedSymbol(_, "_") :: restPattern,
            _ :: restOperands) =>
        // They used a wildcard - ignore this
        matchRule(literals, restPattern, restOperands)

      case (sst.ScopedSymbol(_, patternIdent) :: restPattern, 
            sst.ScopedSymbol(_, operandIdent) :: restOperands) if literals.contains(patternIdent) =>
        if (patternIdent == operandIdent) {
          // The literal doesn't cause any rewrites. We just ignore it
          matchRule(literals, restPattern, restOperands)
        }
        else {
          // They misused a literal - no match
          None
        }

      case (sst.ScopedSymbol(patternScope, patternIdent) :: sst.ScopedSymbol(_, "...") :: restPattern,
            allOperands) if (!literals.contains("...")) =>
        if (allOperands.length >= restPattern.length) {
          val (operands, restOperands) = allOperands.splitAt(allOperands.length - restPattern.length)

          // If the match doesn't fail later add our rewrite rule on
          matchRule(literals, restPattern, restOperands) map { rewrites =>
            SpliceRewrite(patternScope, patternIdent, operands) :: rewrites
          }
        }
        else {
          // Not enough values left
          None
        }

      case (sst.ScopedSymbol(patternScope, patternIdent) :: restPattern,
            operand :: restOperands) =>
        matchRule(literals, restPattern, restOperands) map { rewrites =>
          SubstituteRewrite(patternScope, patternIdent, operand) :: rewrites
        }

      case (sst.ScopedProperList(subpattern) :: restPattern,
            sst.ScopedProperList(suboperands) :: restOperands) =>
        // Recurse inside the proper list and the rest of our pattern
        for(subRewrites <- matchRule(literals, subpattern, suboperands);
            restRewrites <- matchRule(literals, restPattern, restOperands)) 
          yield subRewrites ++ restRewrites
      
      case (sst.ScopedVectorLiteral(subpattern) :: restPattern,
            sst.ScopedVectorLiteral(suboperands) :: restOperands) =>
        // Recurse inside the vector and the rest of our pattern
        for(subRewrites <- matchRule(literals, subpattern.toList, suboperands.toList);
            restRewrites <- matchRule(literals, restPattern, restOperands)) 
          yield subRewrites ++ restRewrites
      
      case (sst.ScopedImproperList(subpattern, termPattern) :: restPattern,
            sst.ScopedImproperList(suboperands, termOperand) :: restOperands) =>
        // This is essentially the same as the proper list except with terminator matching
        for(subRewrites <- matchRule(literals, subpattern, suboperands);
            termRewrites <- matchRule(literals, List(termPattern), List(termOperand));
            restRewrites <- matchRule(literals, restPattern, restOperands)) 
          yield subRewrites ++ termRewrites ++ restRewrites

      case ((patternAtom : sst.NonSymbolLeaf) :: restPattern,
            (operandAtom : sst.NonSymbolLeaf) :: restOperands) if patternAtom == operandAtom => 
        // Operand match
        matchRule(literals, restPattern, restOperands)

      case (Nil, Nil) =>
        // We both ended at the same time - this is expected
        Some(Nil)

      case _ =>
        None
    }
  }

  def expandTemplate(template : sst.ScopedDatum, rewrites : List[Rewrite]) : sst.ScopedDatum = {
    for(rewrite <- rewrites) {
      rewrite match {
        case SpliceRewrite(scope, identifier, expansion) =>
          template match {
            case sst.ScopedPair(sst.ScopedSymbol(symScope, symIdentifier), sst.ScopedPair(sst.ScopedSymbol(_, "..."), cdr)) =>
              if ((symScope == scope) && (symIdentifier == identifier)) {
                val expandedCdr = expandTemplate(cdr, rewrites)

                return expansion.foldRight(expandedCdr) { (car, cdr) =>
                  sst.ScopedPair(car, cdr) 
                }
              }

            case _ =>
              // Nothing
          }

        case SubstituteRewrite(scope, identifier, expansion) =>
          if (template == sst.ScopedSymbol(scope, identifier)) {
            // The expansion should be already fully expanded - we don't need to recurse
            return expansion
          }
      }
    }

    template match {
      // Escaped ellipsis
      case sst.ScopedProperList(sst.ScopedSymbol(scope, "...") :: sst.ScopedSymbol(_, "...") :: Nil) =>
        sst.ScopedSymbol(scope, "...")

      case sst.ScopedPair(car, cdr) =>
        sst.ScopedPair(expandTemplate(car, rewrites), expandTemplate(cdr, rewrites))

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

