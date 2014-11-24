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
      operandList : List[sst.ScopedDatum],
      operandTerminator : sst.ScopedDatum
  ) : ParsedFormals = {
    val (fixedArgData, restArgNameOpt, restArgMemberTypeOpt) =
      (operandList.reverse, operandTerminator) match {
        // This looks for a terminal rest arg in the form: name : <type> *
        case (
            sst.ScopedSymbol(_, "*") ::
            (restArgType : sst.ScopedSymbol) ::
            (annSymbol : sst.ScopedSymbol) ::
            (restArgName : sst.ScopedSymbol) ::
            reverseFixedArgs
          , sst.NonSymbolLeaf(ast.EmptyList())
        ) if annSymbol.resolve == Primitives.AnnotateStorageLocType =>
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
    val fixedOperands = fixedArgData.map {
      case sst.ScopedProperList(List(scopedSymbol : sst.ScopedSymbol, annSymbol : sst.ScopedSymbol, typeDatum))
          if annSymbol.resolve == Primitives.AnnotateStorageLocType =>
        scopedSymbol -> Some(ExtractType.extractNonEmptySchemeType(typeDatum))

      case scopedSymbol : sst.ScopedSymbol =>
        scopedSymbol -> None

      case datum =>
        val message = s"Unrecognized operand definition. Must be either identiifer or [identifier : <type>]."
        throw new BadSpecialFormException(datum, message)
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
