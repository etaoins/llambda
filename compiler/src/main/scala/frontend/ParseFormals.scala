package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

case class ParsedFormals(
    fixedOperands : List[(sst.ScopedSymbol, Option[vt.SchemeType])],
    restOperandOpt : Option[(sst.ScopedSymbol, Option[vt.SchemeType])]
)

object ParseFormals {
  def apply(
      typed : Boolean,
      operandList : List[sst.ScopedDatum],
      operandTerminator : sst.ScopedDatum
  ) : ParsedFormals = {
    val (fixedArgData, restArgNameOpt, restArgMemberTypeOpt) =
      (operandList.reverse, operandTerminator) match {
        // This looks for a terminal rest arg in the form: name : <type> *
        case (
            sst.ScopedSymbol(_, "*") ::
            (restArgType : sst.ScopedSymbol) ::
            (typeColon @ sst.ScopedSymbol(_, ":")) ::
            (restArgName : sst.ScopedSymbol) ::
            reverseFixedArgs
        , sst.NonSymbolLeaf(ast.EmptyList())) if typed =>
          // This is a typed rest argument
          (reverseFixedArgs.reverse, Some(restArgName), Some(ExtractType.extractNonEmptySchemeType(restArgType)))

        case (_, restArgSymbol : sst.ScopedSymbol) =>
          // This has an untyped rest argument
          (operandList, Some(restArgSymbol), None)

        case (_, sst.NonSymbolLeaf(ast.EmptyList())) =>
          // This has no rest argument
          (operandList, None, None)

        case (_, datum) =>
          throw new BadSpecialFormException(datum, "Rest argument expected")
      }

    // Find the types in our signature
    val fixedOperands = if (typed) {
      fixedArgData.map {
        case sst.ScopedProperList(List(scopedSymbol : sst.ScopedSymbol, sst.ScopedSymbol(_, ":"), typeDatum)) =>
          scopedSymbol -> Some(ExtractType.extractNonEmptySchemeType(typeDatum))

        case other =>
          throw new BadSpecialFormException(other, "Expected (symbol : <type>)")
      }
    }
    else {
      fixedArgData.map {
        case scopedSymbol : sst.ScopedSymbol =>
          scopedSymbol -> None

        case datum =>
          throw new BadSpecialFormException(datum, "Symbol expected")
      }
    }

    val restOperandOpt = restArgNameOpt map { restArgName =>
      restArgName -> restArgMemberTypeOpt
    } : Option[(sst.ScopedSymbol, Option[vt.SchemeType])]

    ParsedFormals(
      fixedOperands=fixedOperands,
      restOperandOpt=restOperandOpt
    )
  }
}
