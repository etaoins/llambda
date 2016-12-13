package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

object ResolveImportDecl {
  private def parseIdentifier(datum: ast.Datum): String = datum match {
    case ast.Symbol(name) => name
    case other => throw new BadSpecialFormException(datum, "Symbol expected")
  }

  private def resolveImportSet(datum: ast.Datum)(implicit libraryLoader: LibraryLoader, frontendConfig: FrontendConfig): Map[String, BoundValue] = datum match {
    case ast.ProperList(ast.Symbol("only") :: childSet :: identifierHead :: identifierTail) =>
      val childBindings = resolveImportSet(childSet)

      (identifierHead :: identifierTail).map({ identifierDatum =>
        val identifier = parseIdentifier(identifierDatum)

        val boundValue = childBindings.getOrElse(identifier, {
          throw new ImportedIdentifierNotFoundException(identifierDatum, identifier)
        })

        (identifier -> boundValue)
      }).toMap

    case ast.ProperList(ast.Symbol("except") :: childSet :: identifierHead :: identifierTail) =>
      val childBindings = resolveImportSet(childSet)

      (identifierHead :: identifierTail).foldLeft(childBindings) { (bindings, identifierDatum) =>
        val identifier = parseIdentifier(identifierDatum)

        if (!bindings.contains(identifier)) {
          throw new ImportedIdentifierNotFoundException(identifierDatum, identifier)
        }

        bindings - identifier
      }

    case ast.ProperList(ast.Symbol("prefix") :: childSet :: ast.Symbol(prefix) :: Nil) =>
      resolveImportSet(childSet) map {
        case (name, boundValue) => (prefix + name -> boundValue)
      }

    case ast.ProperList(ast.Symbol("rename") :: childSet :: renameData) =>
      val childBindings = resolveImportSet(childSet)

      renameData.foldLeft(childBindings) { case (bindings, renameDatum) =>
        renameDatum match {
          // Convert to from -> to pairs
          case ast.ProperList(ast.Symbol(from) :: ast.Symbol(to) :: Nil) =>
            // Make sure the renamed identifier exists
            val boundValue = bindings.getOrElse(from, {
              throw new ImportedIdentifierNotFoundException(renameDatum, from)
            })

            bindings - from + (to -> boundValue)

          case other =>
            throw new BadSpecialFormException(other, "Rename expected")
        }
      }

    case libraryNameDatum =>
      libraryLoader.load(ParseLibraryName(libraryNameDatum), libraryNameDatum)
  }

  def apply(datum: ast.Datum)(implicit libraryLoader: LibraryLoader, frontendConfig: FrontendConfig): Map[String, BoundValue] = datum match {
    case ast.ProperList(ast.Symbol("import") :: importSets) =>
      importSets.flatMap(resolveImportSet).toMap

    case _ =>
      throw new BadSpecialFormException(datum, "Unable to parse import declaration");
  }
}

