package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import util.control.Exception._

import annotation.tailrec

private[frontend] object ExpandMacro {
  private case class MatchConfig(
    ellipsisIdentifier : String,
    literals : Set[SyntaxVariable]
  ) {
    /** Indicates if "..." matches should be allowed */
    val zeroOrMoreAllowed = 
      !literals.contains(UnboundSyntaxVariable(ellipsisIdentifier))

    /** Indicates if _ matches should be allowed */
    val wildcardAllowed = 
      !literals.contains(UnboundSyntaxVariable("_"))
  }

  private case class Expandable(template : sst.ScopedDatum, matchedPattern : MatchedPattern)
  
  private class MatchFailedException extends Exception
  private class PatternVariablesExhaustedException extends Exception

  private type MatchedPattern = Map[MacroPatternVariable, Vector[sst.ScopedDatum]]

  private def matchExactlyOnce(matchConfig : MatchConfig, patternDatum : sst.ScopedDatum, operandDatum : sst.ScopedDatum) : MatchedPattern = 
    (patternDatum, operandDatum) match {
      case (sst.ScopedSymbol(_, "_"), anything) if matchConfig.wildcardAllowed =>
        // Consume the wildcard without producing any data
        Map()

      case ((patternSymbol : sst.ScopedSymbol), (operandDatum : sst.ScopedDatum)) =>
        val literals = matchConfig.literals
        val patternSyntaxVariable = SyntaxVariable.fromSymbol(patternSymbol)

        val operandSyntaxVariableOpt = operandDatum match {
          case operandSymbol : sst.ScopedSymbol =>
            Some(SyntaxVariable.fromSymbol(operandSymbol))

          case _ =>
            None
        }

        val patternIsLiteral = literals.contains(patternSyntaxVariable)
        val operandIsLiteral = operandSyntaxVariableOpt.map(literals.contains(_)).getOrElse(false)

        if (patternIsLiteral != operandIsLiteral) {
          // Either both or neither have to be literals
          throw new MatchFailedException
        }

        if (patternIsLiteral) {
          if (patternSyntaxVariable != operandSyntaxVariableOpt.get) {
            // They were both literals but different literals
            throw new MatchFailedException
          }

          // No data to contribute
          Map()
        }
        else {
          // We have a new value!
          val newMacroPatternVariable = MacroPatternVariable(0, patternSyntaxVariable) 
          Map(newMacroPatternVariable -> Vector(operandDatum))
        }

      case (sst.ScopedProperList(innerPattern), sst.ScopedProperList(innerOperands)) =>
        matchPattern(matchConfig, innerPattern, innerOperands)

      case (sst.ScopedImproperList(innerPattern, termPattern), sst.ScopedImproperList(innerOperands, termOperand)) =>
        // Match the terminator separately
        val innerVariables = matchPattern(matchConfig, innerPattern, innerOperands)
        val termVariables = matchPattern(matchConfig, List(termPattern), List(termOperand))
        
        innerVariables ++ termVariables
      
      case (sst.ScopedVectorLiteral(innerPattern), sst.ScopedVectorLiteral(innerOperands)) =>
        matchPattern(matchConfig, innerPattern.toList, innerOperands.toList)

      case ((patternLeaf : sst.NonSymbolLeaf), (operandLeaf : sst.NonSymbolLeaf)) =>
        if (patternLeaf != operandLeaf) {
          throw new MatchFailedException
        }

        // No data to produce
        Map()

      case _ =>
        throw new MatchFailedException
    }
  
  private def matchPattern(matchConfig : MatchConfig, patternData : List[sst.ScopedDatum], operandData : List[sst.ScopedDatum]) : MatchedPattern = {
    (patternData, operandData) match {
      case (subpatternDatum :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: patternTail,
            _
           ) if matchConfig.zeroOrMoreAllowed =>
        // Find all the pattern variables
        // This is so we can remove the corresponding repeating templates if there are no matches
        val subpatternVariables = ParseSyntaxDefine.findPatternVariables(matchConfig.ellipsisIdentifier, matchConfig.literals, List(subpatternDatum), 0)

        // Take in to account any fixed patterns after us
        val (subpatternOperands, restOperands) = operandData.splitAt(operandData.length - patternTail.length)

        // Go over each operand and match
        val submatches = subpatternOperands.map(matchExactlyOnce(matchConfig, subpatternDatum, _)) 

        // Flatten the submatch data together
        val submatchVariables = (subpatternVariables.map { patternVariable =>
          // Get the value from each submatch
          val mergedData = submatches.flatMap(_.apply(patternVariable)).toVector

          // Increment the ellipsis count
          val ellipsisDepth = patternVariable.ellipsisDepth + 1

          // Put the merged data back
          (MacroPatternVariable(ellipsisDepth, patternVariable.variable) -> mergedData)
        }).toMap

        // Run on the rest of our operands
        submatchVariables ++ matchPattern(matchConfig, patternTail, restOperands)

      case (patternDatum :: patternTail, operandDatum :: operandTail) => 
        val newVariables = matchExactlyOnce(matchConfig, patternDatum, operandDatum)
        newVariables ++ matchPattern(matchConfig, patternTail, operandTail)

      case (Nil, Nil) => 
        // We terminated at the same time - this is expected
        Map()

      case _ =>
        // Something weird happened
        throw new MatchFailedException
    }
  }

  private def expandRepeatingTemplate(ellipsisDepth : Int, valueIndex : Int, template : sst.ScopedDatum)(implicit matchConfig : MatchConfig, matchedPattern : MatchedPattern) : List[sst.ScopedDatum] = {
    @tailrec
    def expandRepeatingTemplateAcc(ellipsisDepth : Int, valueIndex : Int, template : sst.ScopedDatum, acc : List[sst.ScopedDatum] = Nil) : List[sst.ScopedDatum] = {
      catching(classOf[PatternVariablesExhaustedException]) opt expandTemplateWithIndices(ellipsisDepth, valueIndex, template) match {
        case Some((usedVariable, currentExpansion)) =>
          if (!usedVariable) {
            throw new BadSpecialFormException(template, "Repeating template does not contain pattern variables")
          }

          expandRepeatingTemplateAcc(ellipsisDepth, valueIndex + 1, template, currentExpansion :: acc)

        case None =>
          acc
      }
    }

    expandRepeatingTemplateAcc(ellipsisDepth, valueIndex, template).reverse
  }

  private def expandVectorElements(ellipsisDepth : Int, valueIndex : Int, elements : List[sst.ScopedDatum])(implicit matchConfig : MatchConfig, matchedPattern : MatchedPattern) : (Boolean, List[sst.ScopedDatum]) = {
    elements match {
      case subtemplate :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: tailTemplate if matchConfig.zeroOrMoreAllowed =>
        val replacements = expandRepeatingTemplate(ellipsisDepth + 1, 0, subtemplate)

        // Splice in the replacements
        // This had to have used a variale or expandRepeatingTemplate would've thrown an exception
        (true, replacements ++ expandVectorElements(ellipsisDepth, valueIndex, tailTemplate)._2)

      case templateDatum :: tailTemplate =>
        val (usedVariable, replacement) = expandTemplateWithIndices(ellipsisDepth + 1, 0, templateDatum)

        // Replace this element only
        val (restUsedVariable, restReplacements) = expandVectorElements(ellipsisDepth, valueIndex, tailTemplate)

        (usedVariable || restUsedVariable, replacement :: restReplacements)

      case Nil =>
        (false, Nil)
    }
  }

  private def expandTemplateWithIndices(ellipsisDepth : Int, valueIndex : Int, template : sst.ScopedDatum)(implicit matchConfig : MatchConfig, matchedPattern : MatchedPattern) : (Boolean, sst.ScopedDatum) = {
    // This is "mostly" tail recursive except for when branching for car/cdr
    template match {
      // Avoid sst.ScopedProperList here so we can fail early in the match as an optimization
      case sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), sst.ScopedPair(subtemplate, sst.NonSymbolLeaf(ast.EmptyList()))) if matchConfig.zeroOrMoreAllowed =>
        // Treat ... as literal inside the subtemplate
        val zeroOrMoreDisabledConfig = matchConfig.copy(literals=matchConfig.literals + UnboundSyntaxVariable(matchConfig.ellipsisIdentifier))

        expandTemplateWithIndices(ellipsisDepth, valueIndex, subtemplate)(zeroOrMoreDisabledConfig, matchedPattern)

      case symbol : sst.ScopedSymbol =>
        val patternVariable = MacroPatternVariable(ellipsisDepth, SyntaxVariable.fromSymbol(symbol))

        matchedPattern.get(patternVariable) match {
          case None =>
            (false, symbol)

          case Some(replacementVector) =>
            if (valueIndex >= replacementVector.length) {
              // Our of pattern variables
              throw new PatternVariablesExhaustedException
            }

            (true, replacementVector(valueIndex))
        }
      
      case sst.ScopedPair(subtemplate, sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), cdr)) if matchConfig.zeroOrMoreAllowed =>
        val replacements = expandRepeatingTemplate(ellipsisDepth + 1, 0, subtemplate)
        val (_, expandedCdr) = expandTemplateWithIndices(ellipsisDepth, valueIndex, cdr)

        // Splice the replacements in to the list
        val splicedList = replacements.foldRight(expandedCdr) { (car, cdr) =>
           sst.ScopedPair(car, cdr).assignLocationFrom(template) 
        }

        // This had to have used a variable or expandRepeatingTemplate would've thrown an exception
        (true, splicedList)

      case sst.ScopedPair(car, cdr) =>
        val (carUsedVariable, expandedCar) = expandTemplateWithIndices(ellipsisDepth, valueIndex, car)
        val (cdrUsedVariable, expandedCdr) = expandTemplateWithIndices(ellipsisDepth, valueIndex, cdr)

        val expandedPair = sst.ScopedPair(expandedCar, expandedCdr).assignLocationFrom(template)

        (carUsedVariable || cdrUsedVariable, expandedPair)

      case sst.ScopedVectorLiteral(elements) =>
        val (usedVariable, expandedElements) = expandVectorElements(ellipsisDepth, valueIndex, elements.toList)

        (usedVariable, sst.ScopedVectorLiteral(expandedElements.toVector).assignLocationFrom(template))

      case leaf : sst.NonSymbolLeaf =>
        // There are no symbols here - don't recurse and don't consume any of the pattern
        (false, leaf)
    }
  }

  private def expandTemplate(matchConfig : MatchConfig, template : sst.ScopedDatum, matchedPattern : MatchedPattern) : sst.ScopedDatum = 
    expandTemplateWithIndices(0, 0, template)(matchConfig, matchedPattern)._2

  def apply(syntax : BoundSyntax, operands : List[sst.ScopedDatum], located : SourceLocated) : sst.ScopedDatum = {
    val matchConfig = MatchConfig(
      ellipsisIdentifier=syntax.ellipsisIdentifier,
      literals=syntax.literals
    )

    val expandable = syntax.rules.flatMap { rule =>
      catching(classOf[MatchFailedException]) opt Expandable(rule.template, matchPattern(matchConfig, rule.pattern, operands))
      }.headOption.getOrElse {
        throw new NoSyntaxRuleException(located, operands.map(_.unscope.toString).mkString(" "))
    }

    expandTemplate(matchConfig, expandable.template, expandable.matchedPattern)
  }
}

