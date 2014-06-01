package io.llambda.compiler.frontend
import io.llambda

import scala.annotation.tailrec

import llambda.compiler.sst
import llambda.compiler.BadSpecialFormException

private[frontend] object CondExpander {
  private def requirementSatisfied(requirement : sst.ScopedDatum)(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : Boolean = requirement match {
    case sst.ScopedSymbol(_, name) =>
      frontendConfig.featureIdentifiers.contains(name)

    case sst.ScopedProperList(List(sst.ScopedSymbol(_, "library"), libraryNameDatum)) =>
      val parsedLibName = ParseLibraryName(libraryNameDatum.unscope)
      libraryLoader.exists(parsedLibName)

    case sst.ScopedProperList(List(sst.ScopedSymbol(_, "not"), innerRequirement)) =>
      !requirementSatisfied(innerRequirement)
    
    case sst.ScopedProperList(sst.ScopedSymbol(_, "and") :: innerRequirements) =>
      innerRequirements.foldLeft(true) { (accumulator, innerRequirement) =>
        accumulator && requirementSatisfied(innerRequirement)
      }
    
    case sst.ScopedProperList(sst.ScopedSymbol(_, "or") :: innerRequirements) =>
      innerRequirements.foldLeft(false) { (accumulator, innerRequirement) =>
        accumulator || requirementSatisfied(innerRequirement)
      }

    case other =>
      throw new BadSpecialFormException(other, "Invalid requirement syntax")
  }

  @tailrec
  def apply(clauseList : List[sst.ScopedDatum])(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : List[sst.ScopedDatum] = clauseList match {
    case Nil => 
      // No clauses left; expand to nothing. 
      // The behaviour here is left open by R7RS but expanding to nothing will  evalute to #!unit after being wrapped
      // in an et.Begin which seems sane.
      Nil

    case sst.ScopedProperList((elseSymbol @ sst.ScopedSymbol(_, "else")) :: expandData) :: tailClauses =>
      if (tailClauses.isEmpty) {
        // We hit an else clause as our last clause
        // Expand the else
        expandData
      }
      else {
        // Else was *not* the last clause
        // Don't treat it like a feature identifier
        throw new BadSpecialFormException(elseSymbol, "The else clause must be the last (cond-expand) clause")
      }

    case sst.ScopedProperList(requirement :: expandData) :: tailClauses =>
      if (requirementSatisfied(requirement)) {
        // Requirement satisfied!
        expandData
      }
      else {
        // Keep scanning the clauses
        apply(tailClauses)
      }

    case other :: _ =>
      throw new BadSpecialFormException(other, "(cond-expand) clauses must be proper lists consisting of a requirement followed by zero or more expressions to expand")
  }
}
