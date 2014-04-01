package io.llambda.llvmir

sealed abstract trait ResultDestination {
  protected[llvmir] def asLocalVariable(nameSource : LocalNameSource, irType : FirstClassType) : LocalVariable
}

/** Indicates the desired result location of an instruction */
object ResultDestination {
  /** Indicates the result of an instruction should be place in a newly allocated variable
    *
    * @param  baseName  Name for newly defined variable
    */
  implicit class AutoNamedVariable(baseName : String) extends ResultDestination {
    protected[llvmir] def asLocalVariable(nameSource : LocalNameSource, irType : FirstClassType) : LocalVariable =
      LocalVariable(nameSource.allocate(baseName), irType)
  }

  /** Indicates the result of an instruction should be placed in a predefined variable
    *
    * @param  predefinedVar  Variable to place the instruction result in. The type of this variable must match that of 
    *                        instruction's result.
    */
  implicit class PredefinedVariable(predefinedVar : LocalVariable) extends ResultDestination {
    protected[llvmir] def asLocalVariable(nameSource : LocalNameSource, irType : FirstClassType) : LocalVariable = {
      if (predefinedVar.irType != irType) {
        throw new InconsistentIrException(s"Attempted to store result of type ${irType} in predefined variable of type ${predefinedVar.irType}")
      }

      predefinedVar
    }
  }
}
