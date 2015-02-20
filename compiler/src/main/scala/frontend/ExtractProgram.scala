package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

object ExtractProgram {
  def apply(data : List[ast.Datum])(implicit libraryLoader : LibraryLoader, frontendConfig : FrontendConfig) : List[et.Expr] = {
    // Load any implicit libraries our dialect requires
    val implicitBindings = frontendConfig.schemeDialect.implicitLibraryNames.flatMap(libraryLoader.load(_))

    val filenameOpt = for(datum <- data.headOption;
                          location <- datum.locationOpt;
                          filename <- location.filenameOpt) yield filename

    // Support dialects that want case folded programs
    val caseFoldedData = if (frontendConfig.schemeDialect.caseFoldPrograms) {
      data.map(_.toCaseFolded)
    }
    else {
      data
    }

    // Split out our imports from our expressions
    val (importData, exprData) = caseFoldedData.span {
      case ast.ProperList(ast.Symbol("import") :: _) => true
      case _ => false
    }

    // Find our imported bindings
    val importBindingList = importData.flatMap(ResolveImportDecl(_))

    val initialBindingList = implicitBindings ++ importBindingList

    // Convert it to a scope
    val scope = new Scope(collection.mutable.Map(initialBindingList : _*), None)

    // Extract the program's body expressions
    val debugContext = debug.SourceContext.fromFilenameOpt(filenameOpt)
    val bodyExtractor = new ModuleBodyExtractor(debugContext, libraryLoader, frontendConfig)
    val programExprs = bodyExtractor(exprData, scope)
      
    // Finish our scope
    FinishScope(scope)

    libraryLoader.libraryExprs ++ programExprs
  }
}
