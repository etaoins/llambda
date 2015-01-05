package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

private[frontend] object ParseProcedureTypeConstructor {
  case class Result(
      fixedArgData : List[sst.ScopedDatum],
      restArgMemberDatumOpt : Option[sst.ScopedDatum],
      returnDatum : sst.ScopedDatum
  )

  /** Parses a procedure type constructor in to its components without resolving its types */
  def apply(located : SourceLocated, args : List[sst.ScopedDatum]) : Result = {
    args.reverse match {
      case returnDatum :: sst.ScopedSymbol(_, "*") :: restArgMemberDatum :: reverseFixedArgData =>
        Result(reverseFixedArgData.reverse, Some(restArgMemberDatum), returnDatum)

      case returnDatum :: reverseFixedArgData =>
        Result(reverseFixedArgData.reverse, None, returnDatum)

      case  _ =>
        throw new BadSpecialFormException(located, "-> requires at least one return type argument")
    }
  }
}
