package llambda

class ImportedIdentifierNotFoundException(val identifier : String) 
  extends SemanticException(identifier)

object ResolveImportDecl {
  type LibraryLoader = List[LibraryNameComponent] => Map[String, BoundValue]

  private def parseIdentifier(datum : ast.Datum) : String = datum match {
    case ast.Symbol(name) => name
    case other => throw new BadSpecialFormException("Symbol expected, found " + datum) 
  }

  private def resolveImportSet(datum : ast.Datum)(implicit libraryLoader : LibraryLoader) : Map[String, BoundValue] = datum match {
    case ast.ProperList(ast.Symbol("only") :: childSet :: identifierHead :: identifierTail) =>
      val childBindings = resolveImportSet(childSet) 
      val identifiers = (identifierHead :: identifierTail).map(parseIdentifier)

      (identifiers map { identifer =>
        val boundValue = childBindings.getOrElse(identifer, {
          throw new ImportedIdentifierNotFoundException(identifer)
        })

        (identifer -> boundValue)
      }).toMap
    
    case ast.ProperList(ast.Symbol("except") :: childSet :: identifierHead :: identifierTail) =>
      val childBindings = resolveImportSet(childSet) 
      val identifiers = (identifierHead :: identifierTail).map(parseIdentifier)
      
      identifiers.foldLeft(childBindings) { (bindings, identifier) =>
        if (!bindings.contains(identifier)) {
          throw new ImportedIdentifierNotFoundException(identifier)
        }

        bindings - identifier
      }
    
    case ast.ProperList(ast.Symbol("prefix") :: childSet :: ast.Symbol(prefix) :: Nil) =>
      resolveImportSet(childSet) map {
        case (name, boundValue) => (prefix + name -> boundValue)
      }

    case ast.ProperList(ast.Symbol("rename") :: childSet :: renameData) =>
      val childBindings = resolveImportSet(childSet) 

      (renameData map {
        // Convert to a from -> to pairs
        case ast.ProperList(ast.Symbol(from) :: ast.Symbol(to) :: Nil) =>
          (from -> to)
        case other =>
          throw new BadSpecialFormException("Rename expected, found " + other)
      }).foldLeft(childBindings) { case (bindings, (from, to)) =>
        // Make sure the renamed identifier exists
        val boundValue = bindings.getOrElse(from, { 
          throw new ImportedIdentifierNotFoundException(from)
        })

        // Update the bidings
        bindings - from + (to -> boundValue)
      }

    case libraryNameDatum => 
      libraryLoader(ParseLibraryName(libraryNameDatum))
  }

  def apply(datum : ast.Datum)(implicit libraryLoader : LibraryLoader) : Map[String, BoundValue] = datum match {
    case ast.ProperList(ast.Symbol("import") :: importSets) =>
      importSets.flatMap(resolveImportSet).toMap

    case _ => throw new BadSpecialFormException("Unable to parse import declaration");
  }
}

