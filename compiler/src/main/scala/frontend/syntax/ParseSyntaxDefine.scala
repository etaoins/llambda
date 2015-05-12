package io.llambda.compiler.frontend.syntax
import io.llambda

import llambda.compiler.sst
import llambda.compiler._

private[frontend] object ParseSyntaxDefine {
  private def checkDuplicateVariables(foundVariables : PatternVariables, seenVariables : Set[SyntaxVariable]) : Set[SyntaxVariable] = {
    val afterVisitedSelf = foundVariables.variables.foldLeft(seenVariables) {
      case (seenVariables, _ : BoundSyntaxVariable) =>
        // Allow duplicate bound syntax variables as they must always resolve to the same bound value
        seenVariables

      case (seenVariables, foundVariable) =>
        if (seenVariables.contains(foundVariable)) {
          throw new BadSpecialFormException(foundVariable, "Duplicate pattern variable" + foundVariable.toString)
        }

        seenVariables + foundVariable
    }

    foundVariables.subpatterns.foldLeft(afterVisitedSelf) { case (seenVariables, subpattern) => 
      checkDuplicateVariables(subpattern, seenVariables)
    }
  }

  private def parseTransformers(
      definedSymbol : sst.ScopedSymbol,
      ellipsisVariable : SyntaxVariable,
      literalData : List[sst.ScopedDatum],
      rulesData : List[sst.ScopedDatum],
      debugContext : debug.SourceContext
  ) : (sst.ScopedSymbol, BoundSyntax) = {
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
          ellipsisVariable=ellipsisVariable,
          literals=literals
        )

        val patternVariables = FindPatternVariables(patternDatum)(matchConfig)
          
        checkDuplicateVariables(patternVariables, Set()) 

        Transformer(patternDatum, patternVariables, template)

      case noMatch => throw new BadSpecialFormException(noMatch, "Unable to parse syntax rule")
    }

    val macroLocation = definedSymbol.locationOpt.getOrElse({
      throw new InternalCompilerErrorException("Unable to determine macro location for debug info purposes")
    })

    val macroDebugContext = new debug.SubprogramContext(   
      parentContext=debugContext,
      filenameOpt=macroLocation.filenameOpt,
      startLocation=macroLocation,
      sourceNameOpt=Some(definedSymbol.name)
    )

    (definedSymbol -> new BoundSyntax(ellipsisVariable, literals.toSet, parsedRules, macroDebugContext))
  }

  def apply(
      located : SourceLocated,
      operands : List[sst.ScopedDatum],
      debugContext : debug.SourceContext
  ) : (sst.ScopedSymbol, BoundSyntax) = operands match {
    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ResolvedSymbol(Primitives.SyntaxRules) :: sst.ScopedProperList(literalData) :: rulesData
             )) =>
      parseTransformers(definedSymbol, BoundSyntaxVariable(Primitives.Ellipsis), literalData, rulesData, debugContext)

    case List((definedSymbol : sst.ScopedSymbol),
             sst.ScopedProperList(
               sst.ResolvedSymbol(Primitives.SyntaxRules) ::
               (ellipsisSymbol : sst.ScopedSymbol) ::
               sst.ScopedProperList(literalData) ::
               rulesData
             )) =>
      parseTransformers(definedSymbol, SyntaxVariable.fromSymbol(ellipsisSymbol), literalData, rulesData, debugContext)

    case _ =>
      throw new BadSpecialFormException(located, "Unrecognized (define-syntax) form")
  }
}
