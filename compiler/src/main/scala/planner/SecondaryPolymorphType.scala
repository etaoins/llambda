package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

/** Secondary polymorph field pointing to the primary polymorph */
private[planner] case object SecondaryPolymorphField extends vt.RecordField(
  "primaryPolymorph",
  vt.SchemeTypeAtom(ct.ProcedureCell),
  mutable=false
)

/** Secondary polymorphs are implemented as single field closures */
private[planner] case object SecondaryPolymorphType extends vt.ClosureType(
  "secondyPolymorph",
  List(SecondaryPolymorphField)
)
