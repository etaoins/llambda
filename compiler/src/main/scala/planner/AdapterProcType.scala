package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

/** Value field of procedure adapters */
case object AdapterProcField extends vt.RecordField("targetProc", vt.SchemeTypeAtom(ct.ProcedureCell), mutable=false)

/** Adapter procedures are implemented as single field closures */
case object AdapterProcType extends vt.ClosureType("procAdapter", List(AdapterProcField))
