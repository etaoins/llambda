package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

object ExtractProgram {
  def apply(data : List[ast.Datum])(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : List[et.Expr] = {
    // Split out our imports from our expressions
    val (importData, exprData) = data.span {
      case ast.ProperList(ast.Symbol("import") :: _) => true
      case _ => false
    }
    
    // Find our initial bindings
    val initialBindingList = importData.flatMap(ResolveImportDecl(_))

    // Convert it to a scope
    val scope = new Scope(collection.mutable.Map(initialBindingList : _*), None)

    // Extract the program's body expressions
    val bodyExtractor = new ModuleBodyExtractor(libraryLoader, frontendConfig)
    val programExprs = bodyExtractor(exprData, scope)

    libraryLoader.libraryExprs ++ programExprs
  }
}
