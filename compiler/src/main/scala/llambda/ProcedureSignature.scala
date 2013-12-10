package llambda

import llambda.{valuetype => vt}

trait ProcedureSignature {
  val hasSelfArg : Boolean
  val fixedArgs : List[vt.ValueType]
  val hasRestArg : Boolean
  val returnType : Option[vt.ValueType]
}
