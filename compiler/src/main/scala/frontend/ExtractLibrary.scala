package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import collection.mutable.{ListBuffer, MapBuilder}

private[frontend] object ExtractLibrary {
  private def expandIncludeDecls(datum : ast.Datum)(implicit includePath : IncludePath) : List[(IncludePath, ast.Datum)] = datum match {
    case ast.ProperList(ast.Symbol("include") :: includeNameData) =>
      // Include the files and wrap them in (begin)
      val includeResults = ResolveIncludeList(includeNameData, datum)

      includeResults map { result =>
        (result.innerIncludePath, ast.ProperList(ast.Symbol("begin") :: result.data))
      }
    
    case ast.ProperList(ast.Symbol("include-library-declarations") :: includeNameData) =>
      // Splice the includes in directly
      val includeResults = ResolveIncludeList(includeNameData, datum)
      
      includeResults flatMap { result =>
        result.data flatMap { datum =>
          // Recursively expand inner (include)s
          expandIncludeDecls(datum)(result.innerIncludePath)
        }
      }

    case nonInclude =>
      List((includePath, nonInclude))
  }

  def apply(datum : ast.Datum, expectedName : Option[Seq[LibraryNameComponent]] = None)(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : Library = datum match {
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
      val expandedDecls = decls.flatMap(expandIncludeDecls(_)(frontendConfig.includePath))

      // Library bodies seems to be mostly order-indepenent. This is annoying
      object DeclType extends Enumeration {
        type DeclType = Value
        val Import, Export, Begin = Value
      }

      val groupedDecls = expandedDecls.groupBy { case (_, datum) =>
        datum match {
          // Be cheap and just check the starting symbol here for grouping purposes
          // If it's not a proper list we should notice later on
          case ast.Pair(ast.Symbol("import"), _) => DeclType.Import
          case ast.Pair(ast.Symbol("export"), _) => DeclType.Export
          case ast.Pair(ast.Symbol("begin"), _) => DeclType.Begin
          case other =>
            throw new BadSpecialFormException(other, "Bad library declaration")
        }
      }

      // Resolve all imports
      val importDecls = groupedDecls.getOrElse(DeclType.Import, List())
      val initialBindings = importDecls.map(_._2)flatMap { importDecl =>
        ResolveImportDecl(importDecl)
      }
      
      val scope = new Scope(collection.mutable.Map(initialBindings : _*), None)
      
      // Evaluate all (begin)s and (include)s
      val beginDeclData = groupedDecls.getOrElse(DeclType.Begin, List())

      val expressions = beginDeclData flatMap {
        case (beginIncludePath, ast.ProperList(ast.Symbol("begin") :: exprs)) =>
          val beginConfig = frontendConfig.copy(
            includePath=beginIncludePath
          )

          val bodyExtractor = new ModuleBodyExtractor(libraryLoader, beginConfig)
          bodyExtractor(exprs, scope)

        case (_, other) =>
          throw new BadSpecialFormException(other, "Bad begin declaration")
      }

      // Evaluate exports to determine our exported bindings
      val exportDecls = groupedDecls.getOrElse(DeclType.Export, List())

      // Flatten out our decls
      val exportSpecs = exportDecls map(_._2) flatMap {
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

