package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.debug

case class FrontendContext(
    config: FrontendConfig,
    libraryLoader: LibraryLoader,
    debugContext: debug.SourceContext
)
