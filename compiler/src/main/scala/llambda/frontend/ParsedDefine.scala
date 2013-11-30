package llambda.frontend

import llambda.sst
import llambda.et
import llambda.{valuetype => vt}
import llambda.{BoundSyntax, StorageLocation}

abstract sealed class ParsedDefine

case class ParsedVarDefine(name : sst.ScopedSymbol, value : StorageLocation, expr : () => et.Expression) extends ParsedDefine
case class ParsedSyntaxDefine(name : sst.ScopedSymbol, value : BoundSyntax) extends ParsedDefine
case class ParsedRecordTypeDefine(typeSymbol : sst.ScopedSymbol, recordDataType : vt.RecordDataType, procedures : Map[sst.ScopedSymbol, et.RecordTypeProcedure]) extends ParsedDefine
