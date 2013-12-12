package io.llambda.compiler.frontend
import io.llambda

import util.control.Exception._
import llambda.compiler._

private[frontend] object ExpandMacro {
  sealed abstract class Rewrite
  case class SubstituteRewrite(scope : Scope, identifier : String, expansion : sst.ScopedDatum) extends Rewrite
  case class SpliceRewrite(scope : Scope, identifier : String, expansion : List[sst.ScopedDatum]) extends Rewrite

  case class Expandable(template : sst.ScopedDatum, rewrites : List[Rewrite])

  // This is used for flow control which is a bit icky
  // We will continue on trying to match the next syntax rule if this is thrown
  private class MatchFailedException extends Exception

  private def findAllSymbols(datum : sst.ScopedDatum) : List[sst.ScopedSymbol] = datum match {
    case symbol : sst.ScopedSymbol => symbol :: Nil
    case sst.ScopedPair(car, cdr) => findAllSymbols(car) ++ findAllSymbols(cdr)
    case sst.ScopedVectorLiteral(elements) => elements.toList.flatMap(findAllSymbols(_))
    case _ : sst.NonSymbolLeaf => Nil
  }
  
  private def matchNonRepeatingRule(literals : List[String], pattern : sst.ScopedDatum, operand : sst.ScopedDatum) : List[Rewrite] = {
    (pattern, operand) match {
      case (sst.ScopedSymbol(_, "_"), _) =>
        // They used a wildcard - ignore this
        Nil

      case (sst.ScopedSymbol(_, patternIdent), 
            sst.ScopedSymbol(_, operandIdent)) if literals.contains(patternIdent) =>
        if (patternIdent == operandIdent) {
          // The literal doesn't cause any rewrites. We just ignore it
          Nil
        }
        else {
          // They misused a literal - no match
          throw new MatchFailedException
        }

      case (sst.ScopedSymbol(patternScope, patternIdent), operand) =>
        List(SubstituteRewrite(patternScope, patternIdent, operand))

      case (sst.ScopedProperList(subpattern), sst.ScopedProperList(suboperands)) =>
        // Recurse inside the proper list
        matchRule(literals, subpattern, suboperands)
      
      case (sst.ScopedVectorLiteral(subpattern), sst.ScopedVectorLiteral(suboperands)) =>
        // Recurse inside the vector and the rest of our pattern
        matchRule(literals, subpattern.toList, suboperands.toList)
      
      case (sst.ScopedImproperList(subpattern, termPattern), sst.ScopedImproperList(suboperands, termOperand)) =>
        // This is essentially the same as the proper list except with terminator matching
        matchRule(literals, subpattern, suboperands) ++ matchRule(literals, List(termPattern), List(termOperand))

      case ((patternAtom : sst.NonSymbolLeaf), (operandAtom : sst.NonSymbolLeaf)) if patternAtom == operandAtom => 
        Nil

      case _ =>
        // No match!
        throw new MatchFailedException
    }
  }

  private def matchRule(literals : List[String], patterns : List[sst.ScopedDatum], operands : List[sst.ScopedDatum]) : List[Rewrite] = {
    (patterns, operands) match {
      case (subpattern :: sst.ScopedSymbol(_, "...") :: restPattern,
            allOperands) if (!literals.contains("...")) =>

        if (allOperands.length == restPattern.length) {
          val allSymbols = findAllSymbols(subpattern)

          // Super gross
          // We work by incrementally matching our pattern against our input and creating rewrites as we go
          // However, in the case of a zero or more match we have to generate the rewrites without any input to match
          // against. Just look for all the identifiers in the subpattern and filter out the literals
          allSymbols filterNot { symbol =>
            literals.contains(symbol.name)
          } map { symbol =>
            SpliceRewrite(symbol.scope, symbol.name, Nil)
          }
        }
        else if (allOperands.length >= restPattern.length) {
          val (repeatingOperands, restOperands) = allOperands.splitAt(allOperands.length - restPattern.length)

          val repeatingRewrites = (repeatingOperands map { repeatingOperand =>
            matchNonRepeatingRule(literals, subpattern, repeatingOperand) 
          }).flatten : List[Rewrite]

          // Fail if there are any splice rewrites inside the list
          // Not sure how that would be handled
          val repeatingSubstitutions = repeatingRewrites.map { 
            case substitution : SubstituteRewrite => substitution
            case _ => throw new MatchFailedException
          } : List[SubstituteRewrite]
          
          val groupedSubstitutions = repeatingSubstitutions.groupBy { substitution =>
            (substitution.scope, substitution.identifier)
          }
            
          // Convert the substitution rewrite list in to splice rewrites
          (groupedSubstitutions.toList map {
            case ((scope, identifier), rewrites) =>
              SpliceRewrite(scope, identifier, rewrites.map(_.expansion))
          }) ++ matchRule(literals, restPattern, restOperands)
        }
        else {
          // Not enough values left
          throw new MatchFailedException
        }

      case (pattern :: restPatterns, operand :: restOperands) =>
        matchNonRepeatingRule(literals, pattern, operand) ++ matchRule(literals, restPatterns, restOperands)

      case (Nil, Nil) =>
        // We both ended at the same time - this is expected
        Nil

      case _ =>
        throw new MatchFailedException
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
      case sst.ScopedProperList(sst.ScopedSymbol(scope, "...") :: datum :: Nil) =>
        // Remove all the splice rewrite rules
        val nonSpliceRewrites = rewrites.filterNot(_.isInstanceOf[SpliceRewrite])
        expandTemplate(datum, nonSpliceRewrites)

      case sst.ScopedPair(car, cdr) =>
        sst.ScopedPair(expandTemplate(car, rewrites), expandTemplate(cdr, rewrites))

      case _ =>
        template
    }
  }

  def apply(syntax : BoundSyntax, operands : List[sst.ScopedDatum], located : SourceLocated) : sst.ScopedDatum = {
    val expandable = syntax.rules.flatMap { rule =>
      catching(classOf[MatchFailedException]) opt Expandable(rule.template, matchRule(syntax.literals, rule.pattern, operands))
    }.headOption.getOrElse {
      throw new NoSyntaxRuleException(located, operands.map(_.unscope.toString).mkString(" "))
    }

    expandTemplate(expandable.template, expandable.rewrites)
  }
}

