package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler._
import util.control.Exception._

private[frontend] object ExpandMacro {
  private case class Expandable(transformer : Transformer, matchedData : MatchedData)
  
  private class MatchFailedException extends Exception

  private case class MatchedData(
    variableData : Map[SyntaxVariable, sst.ScopedDatum] = Map(),
    subpatternData : Vector[List[MatchedData]] = Vector()
  ) {
    def ++(other : MatchedData) : MatchedData = 
      MatchedData(
        variableData=variableData ++ other.variableData,
        subpatternData=subpatternData ++ other.subpatternData
      )
  }
  
  private def matchPatternListWithRest(patternData : List[sst.ScopedDatum], operandData : List[sst.ScopedDatum])(implicit matchConfig : MatchConfig) : (List[sst.ScopedDatum], MatchedData) = {
    (patternData, operandData) match {
      case (subpatternDatum :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: patternTail,
            _
           ) if matchConfig.zeroOrMoreAllowed =>
        // Take in to account any fixed patterns after us
        val (subpatternOperands, fixedTailOperands) = operandData.splitAt(operandData.length - patternTail.length)

        // Go over each operand and match
        val submatches = subpatternOperands.map(matchPattern(subpatternDatum, _)) 

        // Add the subpattern in 
        val submatchData = MatchedData(
          subpatternData=Vector(submatches)
        )

        // Run on the rest of our operands
        val (restOperands, tailData) = matchPatternListWithRest(patternTail, fixedTailOperands) 

        (restOperands, submatchData ++ tailData)

      case (patternDatum :: patternTail, operandDatum :: operandTail) => 
        val newVariables = matchPattern(patternDatum, operandDatum)
        val (restOperands, tailData) = matchPatternListWithRest(patternTail, operandTail)

        (restOperands, newVariables ++ tailData) 

      case (Nil, restOperands) => 
        // We terminated at the same time - this is expected
        (restOperands, MatchedData())

      case _ =>
        // Something weird happened
        throw new MatchFailedException
    }
  }
  
  // Same as matchPatternListWithRest except it throws a MatchFailedException if there are rest operamds
  private def matchPatternList(patternData : List[sst.ScopedDatum], operandData : List[sst.ScopedDatum])(implicit matchConfig : MatchConfig) : MatchedData = {
    val (restOperands, matchData) = matchPatternListWithRest(patternData, operandData)

    if (restOperands != Nil) {
      throw new MatchFailedException
    }

    matchData
  }

  private def matchPattern(patternDatum : sst.ScopedDatum, operandDatum : sst.ScopedDatum)(implicit matchConfig : MatchConfig) : MatchedData = 
    (patternDatum, operandDatum) match {
      case (sst.ScopedSymbol(_, "_"), anything) if matchConfig.wildcardAllowed =>
        // Consume the wildcard without producing any data
        MatchedData()

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
          MatchedData()
        }
        else {
          // We have a new value!
          MatchedData(
            variableData=Map(patternSyntaxVariable -> operandDatum)
          )
        }

      case (sst.ScopedProperList(innerPattern), sst.ScopedProperList(innerOperands)) =>
        matchPatternList(innerPattern, innerOperands)

      case (sst.ScopedImproperList(innerPattern, termPattern), sst.ScopedAnyList(innerOperands, termOperand)) =>
        // Match the terminator separately
        val (restOperands, innerVariables) = matchPatternListWithRest(innerPattern, innerOperands)

        // Build a fake list with the rest of the operands and the terminator
        val fakeRestList = sst.ScopedAnyList(restOperands, termOperand)
        val termVariables = matchPatternList(List(termPattern), List(fakeRestList))
        
        innerVariables ++ termVariables
      
      case (sst.ScopedVectorLiteral(innerPattern), sst.ScopedVectorLiteral(innerOperands)) =>
        matchPatternList(innerPattern.toList, innerOperands.toList)

      case ((patternLeaf : sst.NonSymbolLeaf), (operandLeaf : sst.NonSymbolLeaf)) =>
        if (patternLeaf != operandLeaf) {
          throw new MatchFailedException
        }

        // No data to produce
        MatchedData()

      case _ =>
        throw new MatchFailedException
    }
  
  private def vectorElementsUsePattern(elements : List[sst.ScopedDatum], patternVariables : PatternVariables)(implicit matchConfig : MatchConfig) : Boolean = {
    elements match {
      case subtemplate :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: tailTemplate if matchConfig.zeroOrMoreAllowed =>
        patternVariables.subpatterns.exists(templateUsesPattern(subtemplate, _))

      case templateDatum :: tailTemplate =>
        templateUsesPattern(templateDatum, patternVariables) ||
          vectorElementsUsePattern(tailTemplate, patternVariables)

      case Nil =>
        false
    }
  }

  private def templateUsesPattern(template : sst.ScopedDatum, patternVariables : PatternVariables)(implicit matchConfig : MatchConfig) : Boolean = {
    template match {
      case sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), sst.ScopedPair(subtemplate, sst.NonSymbolLeaf(ast.EmptyList()))) if matchConfig.zeroOrMoreAllowed =>
        // Treat ... as literal inside the subtemplate
        templateUsesPattern(subtemplate, patternVariables)(matchConfig.withoutZeroOrMoreAllowed)

      case symbol : sst.ScopedSymbol =>
        val syntaxVariable = SyntaxVariable.fromSymbol(symbol)
        patternVariables.variables.contains(syntaxVariable)
      
      case sst.ScopedPair(subtemplate, sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), cdr)) if matchConfig.zeroOrMoreAllowed =>
        // Figure out if this matches one of our subpatterns
        patternVariables.subpatterns.exists(templateUsesPattern(subtemplate, _))

      case sst.ScopedPair(car, cdr) =>
        templateUsesPattern(car, patternVariables) || templateUsesPattern(cdr, patternVariables)
      
      case sst.ScopedVectorLiteral(elements) =>
        vectorElementsUsePattern(elements.toList, patternVariables)

      case leaf : sst.NonSymbolLeaf =>
        false
    }
  }

  private def expandRepeatingTemplate(subtemplate : sst.ScopedDatum, patternVariables : PatternVariables, matchedData : MatchedData)(implicit matchConfig : MatchConfig, expandedFrom : SourceLocated) : List[sst.ScopedDatum] = {
    // Figure out if this matches one of our subpatterns
    val possibleSubpatterns = patternVariables.subpatterns.zipWithIndex.filter { case (subpatternVariables, index) =>
      templateUsesPattern(subtemplate, subpatternVariables)
    }

    val (subpatternVariables, subpatternIndex) = possibleSubpatterns match {
      case Vector() =>
       throw new BadSpecialFormException(subtemplate, "Repeating template does not contain pattern variables")

      case Vector(single) =>
        single

      case multiple =>
       throw new BadSpecialFormException(subtemplate, "Repeating template references multiple subpatterns")
    }
        
    matchedData.subpatternData(subpatternIndex) map { submatchedData => 
      expandTemplate(subtemplate, subpatternVariables, submatchedData) 
    }
  }

  private def expandVectorElements(elements : List[sst.ScopedDatum], patternVariables : PatternVariables, matchedData : MatchedData)(implicit matchConfig : MatchConfig, expandedFrom : SourceLocated) : List[sst.ScopedDatum] = {
    elements match {
      case subtemplate :: sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier) :: tailTemplate if matchConfig.zeroOrMoreAllowed =>
        val replacements = expandRepeatingTemplate(subtemplate, patternVariables, matchedData)

        // Splice in the replacements
        replacements ++ expandVectorElements(tailTemplate, patternVariables, matchedData)

      case templateDatum :: tailTemplate =>
        val replacement = expandTemplate(templateDatum, patternVariables, matchedData)

        // Replace this element only
        replacement :: expandVectorElements(tailTemplate, patternVariables, matchedData)

      case Nil =>
        Nil
    }
  }

  private def expandTemplate(template : sst.ScopedDatum, patternVariables : PatternVariables, matchedData : MatchedData)(implicit matchConfig : MatchConfig, expandedFrom : SourceLocated) : sst.ScopedDatum = {
    template match {
      // Avoid sst.ScopedProperList here so we can fail early in the match as an optimization
      case sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), sst.ScopedPair(subtemplate, sst.NonSymbolLeaf(ast.EmptyList()))) if matchConfig.zeroOrMoreAllowed =>
        // Treat ... as literal inside the subtemplate
        expandTemplate(subtemplate, patternVariables, matchedData)(matchConfig.withoutZeroOrMoreAllowed, expandedFrom)

      case symbol : sst.ScopedSymbol =>
        val syntaxVariable = SyntaxVariable.fromSymbol(symbol)

        matchedData.variableData.get(syntaxVariable) match {
          case None =>
            // Pass the pattern symbol through literally
            symbol

          case Some(replacement) =>
            // Replace the variable with the matched data
            replacement
        }
      
      case sst.ScopedPair(subtemplate, sst.ScopedPair(sst.ScopedSymbol(_, matchConfig.ellipsisIdentifier), cdr)) if matchConfig.zeroOrMoreAllowed =>
        // Copy the template for each instance of our matched data
        val replacements = expandRepeatingTemplate(subtemplate, patternVariables, matchedData)

        // Expand our cdr
        val expandedCdr = expandTemplate(cdr, patternVariables, matchedData)

        replacements.foldRight(expandedCdr) { (car, cdr) =>
          sst.ScopedPair(car, cdr)
            .assignLocationFrom(template) 
        }

      case sst.ScopedPair(car, cdr) =>
        val expandedCar = expandTemplate(car, patternVariables, matchedData)
        val expandedCdr = expandTemplate(cdr, patternVariables, matchedData)

        sst.ScopedPair(expandedCar, expandedCdr)
          .assignLocationFrom(template)

      case sst.ScopedVectorLiteral(elements) =>
        val expandedElements = expandVectorElements(elements.toList, patternVariables, matchedData)

        sst.ScopedVectorLiteral(expandedElements.toVector).assignLocationFrom(template)

      case leaf : sst.NonSymbolLeaf =>
        // There are no symbols here - don't recurse and don't consume any of the pattern
        leaf
    }
  }

  def apply(syntax : BoundSyntax, operands : sst.ScopedDatum, expandedFrom : SourceLocated) : sst.ScopedDatum = {
    val matchConfig = MatchConfig(
      ellipsisIdentifier=syntax.ellipsisIdentifier,
      literals=syntax.literals
    )

    val expandable = syntax.transformers.flatMap { transformer =>
      catching(classOf[MatchFailedException]) opt Expandable(transformer, matchPattern(transformer.pattern, operands)(matchConfig))
    }.headOption.getOrElse {
      throw new NoSyntaxRuleException(expandedFrom, operands.unscope.toString)
    }

    val transformer = expandable.transformer
    expandTemplate(transformer.template, transformer.patternVariables, expandable.matchedData)(matchConfig, expandedFrom)
  }
}

