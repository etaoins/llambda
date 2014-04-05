package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.{SyntaxRule, BoundSyntax, BoundSyntaxLiteral, UnboundSyntaxLiteral} 
import llambda.compiler.BadSpecialFormException

private[frontend] object ParseSyntaxDefine {
  def apply(appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : ParsedSimpleDefine = operands match {
    case (symbol : sst.ScopedSymbol) ::
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
             ) :: Nil =>
      val literalNames = literals.map { 
        case symbol @ sst.ScopedSymbol(_, identifier) => 
          symbol.resolveOpt match {
            case Some(boundValue) =>
              BoundSyntaxLiteral(boundValue)

            case None =>
              UnboundSyntaxLiteral(identifier)
          }

        case nonSymbol => throw new BadSpecialFormException(nonSymbol, "Symbol expected in literal list")
      }
      
      val parsedRules = rules map {
        case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
          SyntaxRule(pattern, template)
        case noMatch => throw new BadSpecialFormException(noMatch, "Unable to parse syntax rule")
      }

      ParsedSimpleDefine(symbol, new BoundSyntax(literalNames, parsedRules))

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized (define-syntax) form")
  }
}
