package llambda.frontend

import llambda.sst
import llambda.et
import llambda.{valuetype => vt}
import llambda.{BoundValue, StorageLocation}

abstract sealed class ParsedDefine

case class ParsedVarDefine(name : sst.ScopedSymbol, value : StorageLocation, expr : () => et.Expression) extends ParsedDefine
case class ParsedSimpleDefine(name : sst.ScopedSymbol, value : BoundValue) extends ParsedDefine
case class ParsedRecordTypeDefine(typeSymbol : sst.ScopedSymbol, recordType : vt.RecordCellType, procedures : Map[sst.ScopedSymbol, et.RecordTypeProcedure]) extends ParsedDefine
