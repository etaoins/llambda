package llambda.frontend

import llambda._
import collection.mutable.{ListBuffer, MapBuilder}

private[frontend] object ExtractLibrary {
  private def expandIncludeDecls(datum : ast.Datum)(implicit includePath : IncludePath) : List[(IncludePath, ast.Datum)] = datum match {
    case ast.ProperList(ast.Symbol("include") :: includeNameData) =>
      // Include the files and wrap them in (begin)
      val includeResults = ResolveIncludeList(includeNameData)

      includeResults map { result =>
        (result.innerIncludePath, ast.ProperList(ast.Symbol("begin") :: result.data))
      }
    
    case ast.ProperList(ast.Symbol("include-library-declarations") :: includeNameData) =>
      // Splice the includes in directly
      val includeResults = ResolveIncludeList(includeNameData)
      
      includeResults flatMap { result =>
        result.data flatMap { datum =>
          // Recursively expand inner (include)s
          expandIncludeDecls(datum)(result.innerIncludePath)
        }
      }

    case nonInclude =>
      List((includePath, nonInclude))
  }

  def apply(datum : ast.Datum)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : Library = datum match {
    case ast.ProperList(ast.Symbol("define-library") :: libraryNameData :: decls) =>
      // Parse the library name
      val libraryName = ParseLibraryName(libraryNameData)

      // Expand both types of includes in our first pass
      // Construct the proper include path at the same time so second-order
      // includes from these includes are pathed properly
      val expandedDecls = decls.flatMap(expandIncludeDecls)

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
          case _ =>
            throw new BadSpecialFormException("Bad library declaration: " + datum)
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
          ExtractModuleBody(exprs)(scope, libraryLoader, beginIncludePath)

        case (_, other) =>
          throw new BadSpecialFormException("Bad begin declaration: " + other)
      }

      // Evaluate exports to determine our exported bindings
      val exportDecls = groupedDecls.getOrElse(DeclType.Export, List())

      // Flatten out our decls
      val exportSpecs = exportDecls map(_._2) flatMap {
        case ast.ProperList(ast.Symbol("export") :: decls) => decls
        case other => throw new BadSpecialFormException("Bad export declaration: " + other)
      }

      val exportedBinding = (exportSpecs map {
        case ast.ProperList(ast.Symbol("rename") :: ast.Symbol(internalIdent) :: ast.Symbol(externalIdent) :: Nil) =>
          val boundValue = scope.get(internalIdent).getOrElse {
            throw new UnboundVariableException(internalIdent)
          }

          (externalIdent, boundValue)

        case ast.Symbol(ident) =>
          val boundValue = scope.get(ident).getOrElse {
            throw new UnboundVariableException(ident)
          }

          (ident, boundValue)

        case other =>
          throw new BadSpecialFormException("Bad export specification: " + other)
      }).toMap
      
      Library(libraryName, exportedBinding, expressions)

    case _ =>
      throw new BadSpecialFormException("Bad library declaration: " + datum)
  }
}

