package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{BoundValue, StorageLocation}

abstract sealed class ParsedDefine

case class ParsedVarDefine(
    definedSymbol : sst.ScopedSymbol,
    providedType : Option[vt.SchemeType],
    expr : () => et.Expr,
    storageLocConstructor : (String, vt.SchemeType) => StorageLocation = (new StorageLocation(_, _))
) extends ParsedDefine

case class ValueTarget(
    definedSymbol : sst.ScopedSymbol,
    providedType : Option[vt.SchemeType]
)

case class ParsedMultipleValueDefine(
    fixedValueTargets : List[ValueTarget],
    restValueTargetOpt : Option[ValueTarget],
    expr : () => et.Expr
) extends ParsedDefine

case class ParsedSimpleDefine(definedSymbol : sst.ScopedSymbol, value : BoundValue) extends ParsedDefine
case class ParsedRecordTypeDefine(
    typeSymbol : sst.ScopedSymbol,
    recordType : vt.RecordType,
    procedures : Map[sst.ScopedSymbol, et.ArtificialProcedure]
) extends ParsedDefine

// This doesn't introduce any bound values - it just signals to ExtractModuleBody to not attempt to parse the define
// as a normal expression
case object ParsedTypeAnnotation extends ParsedDefine
