package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

case class ParsedFormals(
    fixedArgs : List[(sst.ScopedSymbol, Option[vt.SchemeType])],
    restArgOpt : Option[(sst.ScopedSymbol, Option[vt.SchemeType])]
)

object ParseFormals {
  def apply(
      argList : List[sst.ScopedDatum],
      argTerminator : sst.ScopedDatum
  ) : ParsedFormals = {
    val (fixedArgData, restArgNameOpt, restArgMemberTypeOpt) =
      (argList.reverse, argTerminator) match {
        // This looks for a terminal rest arg in the form: name : <type> *
        case (
            sst.ScopedSymbol(_, "*") ::
            (restArgType : sst.ScopedSymbol) ::
            sst.ResolvedSymbol(Primitives.AnnotateStorageLocType) ::
            (restArgName : sst.ScopedSymbol) ::
            reverseFixedArgs
          , sst.NonSymbolLeaf(ast.EmptyList())
        ) =>
          // This is a typed rest argument
          (reverseFixedArgs.reverse, Some(restArgName), Some(ExtractType.extractNonEmptySchemeType(restArgType)))

        case (_, restArgSymbol : sst.ScopedSymbol) =>
          // This has an untyped rest argument
          (argList, Some(restArgSymbol), None)

        case (_, sst.NonSymbolLeaf(ast.EmptyList())) =>
          // This has no rest argument
          (argList, None, None)

        case (_, datum) =>
          throw new BadSpecialFormException(datum, "Rest argument expected")
      }

    // Find the types in our signature
    val fixedArgs = fixedArgData.map {
      case sst.ScopedProperList(List(
          scopedSymbol : sst.ScopedSymbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum
      )) =>
        scopedSymbol -> Some(ExtractType.extractNonEmptySchemeType(typeDatum))

      case scopedSymbol : sst.ScopedSymbol =>
        scopedSymbol -> None

      case datum =>
        val message = s"Unrecognized argument definition. Must be either identiifer or [identifier : <type>]."
        throw new BadSpecialFormException(datum, message)
    }

    val restArgOpt = restArgNameOpt map { restArgName =>
      restArgName -> restArgMemberTypeOpt
    } : Option[(sst.ScopedSymbol, Option[vt.SchemeType])]

    ParsedFormals(
      fixedArgs=fixedArgs,
      restArgOpt=restArgOpt
    )
  }
}
