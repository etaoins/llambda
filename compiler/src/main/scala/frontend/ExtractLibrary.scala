package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

private[frontend] object ExtractLibrary {
  private def expandDecls(datum : ast.Datum)(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : List[ast.Datum] = datum match {
    case ast.ProperList(ast.Symbol(includeType @ ("include" | "include-ci")) :: includeNameData) =>
      // Include the files and wrap them in (begin)
      val includeData = ResolveIncludeList(datum, includeNameData)(frontendConfig.includePath)

      val foldedData = if (includeType == "include-ci") {
        includeData.map(_.toCaseFolded)
      }
      else {
        includeData
      }

      List(ast.ProperList(ast.Symbol("begin") :: foldedData))

    case ast.ProperList(ast.Symbol("include-library-declarations") :: includeNameData) =>
      // Splice the includes in directly
      val includeData = ResolveIncludeList(datum, includeNameData)(frontendConfig.includePath)

      // Recursively expand inner decls
      includeData flatMap expandDecls

    case ast.ProperList(ast.Symbol("cond-expand") :: firstClause :: restClauses) =>
      // Evaluate the (cond-expand)
      CondExpander.expandData(firstClause :: restClauses).flatMap(expandDecls)

    case nonExpand =>
      List(nonExpand)
  }

  /** Extracts an Sheme library for a library datum
    *
    * @param  datum         Top-level datum for the library definition
    * @param  expectedName  Expected name for the library. This function will raise LibraryNameMismatchException
    *                       if the library's defined name does not match
    */
  def apply(
      datum : ast.Datum,
      expectedName : Option[Seq[LibraryNameComponent]] = None
  )(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : Library = datum match {
    case ast.ProperList(ast.Symbol("define-library") :: libraryNameData :: decls) =>
      // Parse the library name
      val libraryName = ParseLibraryName(libraryNameData)
    
      for (expected <- expectedName) {
        if (libraryName != expected) {
          throw new LibraryNameMismatchException(libraryNameData, expected, libraryName)
        }
      }

      // Expand both types of includes in our first pass
      // Construct the proper include path at the same time so second-order  includes from these includes are pathed
      // properly
      val expandedDecls = decls.flatMap(expandDecls(_))

      // Library bodies seems to be mostly order-indepenent. This is annoying
      object DeclType extends Enumeration {
        type DeclType = Value
        val Import, Export, Begin = Value
      }

      val groupedDecls = expandedDecls.groupBy {
        // Be cheap and just check the starting symbol here for grouping purposes
        // If it's not a proper list we should notice later on
        case ast.Pair(ast.Symbol("import"), _) => DeclType.Import
        case ast.Pair(ast.Symbol("export"), _) => DeclType.Export
        case ast.Pair(ast.Symbol("begin"), _) => DeclType.Begin
        case other =>
          throw new BadSpecialFormException(other, "Bad library declaration")
      }

      // Resolve all imports
      val importDecls = groupedDecls.getOrElse(DeclType.Import, List())
      val initialBindings = importDecls.flatMap { importDecl =>
        ResolveImportDecl(importDecl)
      }
      
      val scope = new Scope(collection.mutable.Map(initialBindings : _*), None)
      
      // Evaluate all (begin)s and (include)s
      val beginDeclData = groupedDecls.getOrElse(DeclType.Begin, List())

      val expressions = beginDeclData flatMap {
        case ast.ProperList((beginSymbol @ ast.Symbol("begin")) :: exprs) =>
          val filenameOpt = beginSymbol.locationOpt.flatMap(_.filenameOpt)
          val debugContext = debug.SourceContext.fromFilenameOpt(filenameOpt)

          val beginContext = FrontendContext(
            frontendConfig,
            libraryLoader,
            debugContext
          )

          ExtractModuleBody(exprs, scope)(beginContext)

        case other =>
          throw new BadSpecialFormException(other, "Bad begin declaration")
      }

      // Finish our Scope
      FinishScope(scope)

      // Evaluate exports to determine our exported bindings
      val exportDecls = groupedDecls.getOrElse(DeclType.Export, List())

      // Flatten out our decls
      val exportSpecs = exportDecls.flatMap {
        case ast.ProperList(ast.Symbol("export") :: decls) => decls
        case other => throw new BadSpecialFormException(other, "Bad export declaration")
      }

      val exportedBinding = (exportSpecs map {
        case ast.ProperList(ast.Symbol("rename") :: (internalSymbol @ ast.Symbol(internalIdent)) :: ast.Symbol(externalIdent) :: Nil) =>
          val boundValue = scope.get(internalIdent).getOrElse {
            throw new UnboundVariableException(internalSymbol, internalIdent)
          }

          (externalIdent, boundValue)

        case internalSymbol @ ast.Symbol(ident) =>
          val boundValue = scope.get(ident).getOrElse {
            throw new UnboundVariableException(internalSymbol, ident)
          }

          (ident, boundValue)

        case other =>
          throw new BadSpecialFormException(other, "Bad export specification")
      }).toMap

      Library(libraryName, exportedBinding, expressions)

    case other =>
      throw new BadSpecialFormException(other, "Bad library declaration")
  }
}

