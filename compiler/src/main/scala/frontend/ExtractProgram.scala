package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

object ExtractProgram {
  def apply(data : List[ast.Datum])(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : List[et.Expr] = {
    val filenameOpt = for(datum <- data.headOption;
                          location <- datum.locationOpt;
                          filename <- location.filenameOpt) yield filename

    val debugContext = debug.SourceContext.fromFilenameOpt(filenameOpt)

    implicit val frontendContext = FrontendContext(
      config=frontendConfig,
      libraryLoader=libraryLoader,
      debugContext=debugContext
    )

    // Split out our imports from our expressions
    val (importData, exprData) = data.span {
      case ast.ProperList(ast.Symbol("import") :: _) => true
      case _ => false
    }

    // Find our imported bindings
    val importBindingList = importData.flatMap(ResolveImportDecl(_))

    // Convert it to a scope
    val scope = new Scope(collection.mutable.Map(importBindingList : _*), None)

    // Extract the program's body expressions
    val programExprs = ExtractModuleBody(exprData, scope)

    // Finish our scope
    FinishScope(scope)

    libraryLoader.libraryExprs ++ programExprs
  }
}
