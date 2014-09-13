package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

private[frontend] sealed trait ScopedArgument {
  val symbol : sst.ScopedSymbol
  val boundValue : StorageLocation
}

private[frontend] case class ScopedFixedArgument(symbol : sst.ScopedSymbol, boundValue : StorageLocation) extends ScopedArgument

private[frontend] case class ScopedRestArgument(symbol : sst.ScopedSymbol, restArg : et.RestArgument) extends ScopedArgument {
  val boundValue = restArg.storageLoc
}
