package io.llambda.llvmir.debug

import io.llambda.llvmir._

case class LocationMetadata(
  line : Int,
  column : Int,
  scope : Metadata,
  originalScopeOpt : Option[Metadata]
) extends MetadataNode {
  val memberOpts = List(
    Some(IntegerConstant(IntegerType(32), line)),
    Some(IntegerConstant(IntegerType(32), column)),
    Some(scope),
    originalScopeOpt
  )
}
