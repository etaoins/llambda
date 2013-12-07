package llambda.frontend

import llambda.sst
import llambda.{SyntaxRule, BoundSyntax} 
import llambda.BadSpecialFormException

private[frontend] object ParseSyntaxDefine {
  def apply(appliedSymbol : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) : ParsedSimpleDefine = operands match {
    case (symbol : sst.ScopedSymbol) ::
             sst.ScopedProperList(
               sst.ScopedSymbol(_, "syntax-rules") :: sst.ScopedProperList(literals) :: rules
             ) :: Nil =>
      val literalNames = literals.map { 
        case sst.ScopedSymbol(_, name) => name
        case nonSymbol => throw new BadSpecialFormException(nonSymbol, "Symbol expected in literal list")
      }
      
      val parsedRules = rules map {
        case sst.ScopedProperList(sst.ScopedProperList(_ :: pattern) :: template :: Nil) =>
          SyntaxRule(pattern, template)
        case noMatch => throw new BadSpecialFormException(appliedSymbol, "Unable to parse syntax rule")
      }

      ParsedSimpleDefine(symbol, new BoundSyntax(literalNames, parsedRules))

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized (define-syntax) form")
  }
}
