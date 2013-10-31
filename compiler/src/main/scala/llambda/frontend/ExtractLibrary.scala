package llambda.frontend

import llambda._
import collection.mutable.{ListBuffer, MapBuilder}

private[frontend] object ExtractLibrary {
  def apply(datum : ast.Datum)(implicit libraryLoader : LibraryLoader, includePath : IncludePath) : Library = datum match {
    case ast.ProperList(ast.Symbol("define-library") :: libraryNameData :: decls) =>
      // Parse the library name
      val libraryName = ParseLibraryName(libraryNameData)

      // Library bodies seems to be mostly order-indepenent. This is annoying
      object DeclType extends Enumeration {
        type DeclType = Value
        val Import, Export, Begin = Value
      }

      val groupedDecls = decls.groupBy { datum =>
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
      val importDeclData = groupedDecls.getOrElse(DeclType.Import, List())
      val initialBindings = importDeclData.flatMap(ResolveImportDecl(_))
      
      val scope = new Scope(collection.mutable.Map(initialBindings : _*), None)
      
      // Evaluate all (begin)s
      val beginDeclData = groupedDecls.getOrElse(DeclType.Begin, List())

      val expressionData = beginDeclData flatMap {
        case ast.ProperList(ast.Symbol("begin") :: exprs) => exprs
        case other => throw new BadSpecialFormException("Bad begin declaration: " + other)
      }

      val expressions = ExtractModuleBody(expressionData)(scope, includePath)

      // Evaluate exports to determine our exported bindings
      val exportDeclData = groupedDecls.getOrElse(DeclType.Export, List())

      // Flatten out our decls
      val exportSpecData = exportDeclData flatMap {
        case ast.ProperList(ast.Symbol("export") :: decls) => decls
        case other => throw new BadSpecialFormException("Bad export declaration: " + other)
      }

      val exportedBinding = (exportSpecData map {
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

