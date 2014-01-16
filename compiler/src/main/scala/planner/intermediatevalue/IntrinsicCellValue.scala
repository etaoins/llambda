package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.RuntimeErrorMessage

class IntrinsicCellValue(val possibleTypes : Set[ct.ConcreteCellType], val cellType : ct.CellType, val tempValue : ps.TempValue) extends IntermediateCellValue {
  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val truthyTemp = ps.GcUnmanagedValue()

    if (possibleTypes.contains(ct.BooleanCell)) {
      plan.steps += ps.UnboxAsTruthy(truthyTemp, tempValue) 
    }
    else {
      plan.steps += ps.StoreNativeInteger(truthyTemp, 1, 1) 
    }

    truthyTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] =  {
    if (possibleTypes.contains(ct.ProcedureCell)) {
      // Cast to a procedure
      val boxedProcTemp = toTempValue(vt.IntrinsicCellType(ct.ProcedureCell))

      Some(new InvokableProcedureCell(boxedProcTemp))
    }
    else {
      None
    }
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val boxedChar = toTempValue(vt.IntrinsicCellType(ct.CharacterCell))
      val unboxedTemp = ps.GcUnmanagedValue()
      plan.steps += ps.UnboxCharacter(unboxedTemp, boxedChar)

      unboxedTemp
      
    case intType : vt.IntType =>
      val boxedExactInt = toTempValue(vt.IntrinsicCellType(ct.ExactIntegerCell))
      val unboxedTemp = ps.GcUnmanagedValue()
      plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

      if (intType.bits == 64) {
        // Correct width
        unboxedTemp
      }
      else {
        val convTemp = ps.GcUnmanagedValue()

        // Convert to the right width
        plan.steps += ps.ConvertNativeInteger(convTemp, unboxedTemp, intType.bits, intType.signed) 
        convTemp
      }

    case fpType : vt.FpType =>
      val possiblyExactInt = possibleTypes.contains(ct.ExactIntegerCell)
      val possiblyInexactRational = possibleTypes.contains(ct.InexactRationalCell)

      if (!possiblyExactInt && !possiblyInexactRational) {
        // Not possible
        impossibleConversion(s"Unable to convert non-numeric ${typeDescription} to ${fpType.schemeName}")
      }
      else if (possiblyExactInt & !possiblyInexactRational) {
        // Unbox as exact int
        val boxedExactInt = toTempValue(vt.IntrinsicCellType(ct.ExactIntegerCell)) 
        val unboxedTemp = ps.GcUnmanagedValue()
        plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

        // Convert to the wanted type
        val convTemp = ps.GcUnmanagedValue()
        plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, unboxedTemp, true, fpType)

        convTemp
      }
      else if (!possiblyExactInt && possiblyInexactRational) {
        // Unbox as inexact rational
        val boxedInexactRational = toTempValue(vt.IntrinsicCellType(ct.InexactRationalCell)) 
        val unboxedTemp = ps.GcUnmanagedValue()
        plan.steps += ps.UnboxInexactRational(unboxedTemp, boxedInexactRational)

        if (fpType == vt.Double) {
          // No conversion needed
          unboxedTemp
        }
        else {
          val convTemp = ps.GcUnmanagedValue()

          plan.steps += ps.ConvertNativeFloat(convTemp, unboxedTemp, fpType)
          convTemp
        }
      }
      else {
        // We have to check types here and branch on the result
        val isExactIntPred = ps.GcUnmanagedValue()

        plan.steps += ps.TestCellType(isExactIntPred, tempValue, ct.ExactIntegerCell)

        // Try again with constrained types
        // This will hit the branches above us
        val trueWriter = plan.forkPlan()
        val trueDynamicValue = new IntrinsicCellValue(Set(ct.ExactIntegerCell), cellType, tempValue)
        val trueTempValue = trueDynamicValue.toTempValue(fpType)(trueWriter)

        val falseWriter = plan.forkPlan()
        val falseDynamicValue = new IntrinsicCellValue(possibleTypes - ct.ExactIntegerCell, cellType, tempValue)
        val falseTempValue = falseDynamicValue.toTempValue(fpType)(falseWriter)
      
        val phiTemp = ps.GcManagedValue()

        plan.steps += ps.CondBranch(phiTemp, isExactIntPred, 
          trueWriter.steps.toList, trueTempValue,
          falseWriter.steps.toList, falseTempValue) 

        phiTemp
      }

    case vt.CBool =>
      throw new InternalCompilerErrorException("Attempt to directly convert to CBool. Should be caught by toTruthyPredicate.")
  }
  
  protected def toRecordTempValue(recordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = {
    // Convert ourselves to a record
    val recordTemp = toTempValue(vt.IntrinsicCellType(ct.RecordCell))

    // Make sure we we're of the right class
    val errorMessage = errorMessageOpt getOrElse {
      RuntimeErrorMessage(
        name=s"recordClassIsNot${recordType.schemeName}",
        text=s"Runtime cast to record type '${recordType.schemeName}' failed"
      )
    }

    plan.steps += ps.AssertRecordLikeClass(recordTemp, recordType, errorMessage)

    recordTemp
  }
  
  def preferredRepresentation : vt.ValueType = possibleTypes.toList match {
    case singleType :: Nil =>
      // Even if our temp value is currently a supertype we know the cell
      // itself has a more specific type
      vt.IntrinsicCellType(singleType)

    case _ =>
      vt.IntrinsicCellType(cellType)
  }
  
  // Store unboxed where possible to save GC overhead and potentially pack
  // more values in the closure
  // The lifetime of any cells we reference will be at least as long as the
  // procedure cell itself so there's not much point storing them unboxed.
  def closureRepresentation : Option[vt.ValueType] = Some(possibleTypes.toList match {
    case ct.ExactIntegerCell :: Nil => vt.Int64
    case ct.InexactRationalCell :: Nil => vt.Double
    case ct.BooleanCell :: Nil => vt.CBool
    case ct.CharacterCell :: Nil => vt.UnicodeChar
    case _ => preferredRepresentation
  })
}

