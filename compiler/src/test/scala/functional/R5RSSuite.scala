package io.llambda.compiler.functional
import io.llambda
import llambda.compiler.dialect

class R5RSSuite extends SchemeFunctionalTestRunner("R5RSSuite", onlyDialectOpt=Some(dialect.R5RS))
