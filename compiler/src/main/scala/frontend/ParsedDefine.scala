package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{BoundValue, StorageLocation}

abstract sealed class ParsedDefine

case class ParsedVarDefine(definedSymbol : sst.ScopedSymbol, value : StorageLocation, expr : () => et.Expression) extends ParsedDefine
case class ParsedSimpleDefine(definedSymbol : sst.ScopedSymbol, value : BoundValue) extends ParsedDefine
case class ParsedRecordTypeDefine(typeSymbol : sst.ScopedSymbol, recordType : vt.RecordType, procedures : Map[sst.ScopedSymbol, et.RecordTypeProcedure]) extends ParsedDefine
