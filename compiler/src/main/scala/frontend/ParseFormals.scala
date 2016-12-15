package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

case class ParsedOptional(
    symbol: sst.Symbol,
    schemeTypeOpt: Option[vt.SchemeType],
    defaultDatum: sst.ScopedDatum
)


case class ParsedFormals(
    mandatoryArgs: List[(sst.Symbol, Option[vt.SchemeType])],
    optionalArgs: List[ParsedOptional],
    restArgOpt: Option[(sst.Symbol, Option[vt.SchemeType])]
)

object ParseFormals {
  private case class ParsedFixed(
      symbol: sst.Symbol,
      schemeTypeOpt: Option[vt.SchemeType],
      defaultDatumOpt: Option[sst.ScopedDatum]
  )

  def apply(
      argList: List[sst.ScopedDatum],
      argTerminator: sst.ScopedDatum,
      allowOptionals: Boolean = true
  ): ParsedFormals = {
    val (fixedArgData, restArgNameOpt, restArgMemberTypeOpt) =
      (argList.reverse, argTerminator) match {
        // This looks for a terminal rest arg in the form: name: <type> *
        case (
            sst.Symbol(_, "*") ::
            (restArgType: sst.ScopedDatum) ::
            sst.ResolvedSymbol(Primitives.AnnotateStorageLocType) ::
            (restArgName: sst.Symbol) ::
            reverseFixedArgs
          , sst.NonSymbolLeaf(ast.EmptyList())
        ) =>
          // This is a typed rest argument
          (reverseFixedArgs.reverse, Some(restArgName), Some(ExtractType.extractNonEmptySchemeType(restArgType)))

        case (_, restArgSymbol: sst.Symbol) =>
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
      case sst.ProperList(List(
          scopedSymbol: sst.Symbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum,
          defaultDatum
      )) if allowOptionals =>
        ParsedFixed(
          symbol=scopedSymbol,
          schemeTypeOpt=Some(ExtractType.extractNonEmptySchemeType(typeDatum)),
          defaultDatumOpt=Some(defaultDatum)
        )

      case sst.ProperList(List(
          scopedSymbol: sst.Symbol,
          sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
          typeDatum
      )) =>
        ParsedFixed(
          symbol=scopedSymbol,
          schemeTypeOpt=Some(ExtractType.extractNonEmptySchemeType(typeDatum)),
          defaultDatumOpt=None
        )

      case sst.ProperList(List(scopedSymbol: sst.Symbol, defaultDatum)) if allowOptionals =>
        ParsedFixed(
          symbol=scopedSymbol,
          schemeTypeOpt=None,
          defaultDatumOpt=Some(defaultDatum)
        )

      case scopedSymbol: sst.Symbol =>
        ParsedFixed(
          symbol=scopedSymbol,
          schemeTypeOpt=None,
          defaultDatumOpt=None
        )

      case datum =>
        val message = if (allowOptionals) {
          s"Unrecognised argument definition. Must be either identifier, [identifier default], [identifier: <type>] or [identifier: <type> default]."
        }
        else {
          s"Unrecognised argument definition. Must be either identifier or [identifier: <type>]."
        }

        throw new BadSpecialFormException(datum, message)
    }

    val (noDefaultFixed, maybeDefaultFixed) = fixedArgs.span(!_.defaultDatumOpt.isDefined)

    val mandatoryArgs = noDefaultFixed map { case ParsedFixed(symbol, schemeTypeOpt, _) =>
      symbol -> schemeTypeOpt
    }

    val optionalArgs = maybeDefaultFixed map {
      case ParsedFixed(symbol, schemeTypeOpt, Some(defaultDatum)) =>
        ParsedOptional(symbol, schemeTypeOpt, defaultDatum)

      case ParsedFixed(symbol, _, _) =>
        throw new BadSpecialFormException(symbol, "All arguments following an optional argument must have a default")
    }

    val restArgOpt = restArgNameOpt map { restArgName =>
      restArgName -> restArgMemberTypeOpt
    }: Option[(sst.Symbol, Option[vt.SchemeType])]

    ParsedFormals(
      mandatoryArgs=mandatoryArgs,
      optionalArgs=optionalArgs,
      restArgOpt=restArgOpt
    )
  }
}
