package llambda.planner.intermediatevalue

import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}

class IntrinsicCellValue(val possibleTypes : Set[ct.ConcreteCellType], val cellType : ct.CellType, val tempValue : ps.TempValue) extends IntermediateCellValue {
  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val truthyTemp = new ps.TempValue

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
      val boxedProcTmep = toRequiredTempValue(vt.IntrinsicCellType(ct.ProcedureCell))

      Some(new InvokableProcedureCell(boxedProcTmep))
    }
    else {
      None
    }
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = nativeType match {
    case vt.UnicodeChar =>
      toTempValue(vt.IntrinsicCellType(ct.CharacterCell)) map { boxedChar =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxCharacter(unboxedTemp, boxedChar)

        unboxedTemp
      }
      
    case vt.Utf8CString =>
      toTempValue(vt.IntrinsicCellType(ct.StringCell)) map { boxedString =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxStringAsUtf8(unboxedTemp, boxedString)

        unboxedTemp
      }

    case intType : vt.IntType =>
      toTempValue(vt.IntrinsicCellType(ct.ExactIntegerCell)) map { boxedExactInt =>
        val unboxedTemp = new ps.TempValue
        plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

        if (intType.bits == 64) {
          // Correct width
          unboxedTemp
        }
        else {
          val convTemp = new ps.TempValue

          // Convert to the right width
          plan.steps += ps.ConvertNativeInteger(convTemp, unboxedTemp, intType.bits, intType.signed) 
          convTemp
        }
      }

    case fpType : vt.FpType =>
      val possiblyExactInt = possibleTypes.contains(ct.ExactIntegerCell)
      val possiblyInexactRational = possibleTypes.contains(ct.InexactRationalCell)

      if (!possiblyExactInt && !possiblyInexactRational) {
        // Not possible
        None
      }
      else if (possiblyExactInt & !possiblyInexactRational) {
        toTempValue(vt.IntrinsicCellType(ct.ExactIntegerCell)) map { boxedExactInt =>
          // Unbox as exact int
          val unboxedTemp = new ps.TempValue
          plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

          // Convert to the wanted type
          val convTemp = new ps.TempValue
          plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, unboxedTemp, true, fpType)

          convTemp
        }
      }
      else if (!possiblyExactInt && possiblyInexactRational) {
        toTempValue(vt.IntrinsicCellType(ct.InexactRationalCell)) map { boxedInexactRational =>
          // Unbox as inexact rational
          val unboxedTemp = new ps.TempValue
          plan.steps += ps.UnboxInexactRational(unboxedTemp, boxedInexactRational)

          if (fpType == vt.Double) {
            // No conversion needed
            unboxedTemp
          }
          else {
            val convTemp = new ps.TempValue

            plan.steps += ps.ConvertNativeFloat(convTemp, unboxedTemp, fpType)
            convTemp
          }
        }
      }
      else {
        // We have to check types here and branch on the result
        val isExactIntPred = new ps.TempValue

        plan.steps += ps.TestCellType(isExactIntPred, tempValue, ct.ExactIntegerCell)

        // Try again with constrained types
        // This will hit the branches above us
        val trueWriter = plan.forkPlan()
        val trueDynamicValue = new IntrinsicCellValue(Set(ct.ExactIntegerCell), cellType, tempValue)
        val trueTempValue = trueDynamicValue.toRequiredTempValue(fpType)(trueWriter)

        val falseWriter = plan.forkPlan()
        val falseDynamicValue = new IntrinsicCellValue(possibleTypes - ct.ExactIntegerCell, cellType, tempValue)
        val falseTempValue = falseDynamicValue.toRequiredTempValue(fpType)(falseWriter)
      
        val phiTemp = new ps.TempValue

        plan.steps += ps.CondBranch(phiTemp, isExactIntPred, 
          trueWriter.steps.toList, trueTempValue,
          falseWriter.steps.toList, falseTempValue) 

        Some(phiTemp)
      }

    case _ =>
      None
  }
  
  protected def toRecordCellTempValue(recordType : vt.RecordCellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    // Convert ourselves to a record
    val recordTemp = toRequiredTempValue(vt.IntrinsicCellType(ct.RecordCell))

    // Make sure we we're of the right class
    plan.steps += ps.AssertRecordCellClass(recordTemp, recordType)

    Some(recordTemp)
  }
}

