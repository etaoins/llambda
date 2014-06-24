package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure, PlanTypeCheck}
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.RuntimeErrorMessage

class CellValue(val schemeType : vt.SchemeType, val tempType : ct.CellType, val tempValue : ps.TempValue, val properListCell : Boolean = false) extends IntermediateValue {
  override lazy val isDefiniteProperList = properListCell || hasDefiniteCellType(ct.EmptyListCell)

  lazy val typeDescription = s"cell of type ${schemeType}"

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val truthyTemp = ps.Temp(vt.Predicate)

    if (schemeType.satisfiesType(vt.BooleanType) == Some(false)) {
      // We cannot be a boolean
      // That means we must evaluate as true
      plan.steps += ps.CreateNativeInteger(truthyTemp, 1, 1) 
    }
    else {
      plan.steps += ps.UnboxAsTruthy(truthyTemp, tempValue) 
    }

    truthyTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[InvokableProcedure] =  {
    if (schemeType.satisfiesType(vt.ProcedureType) == Some(false)) {
      None
    }
    else {
      // Cast to a procedure
      val boxedProcTemp = toTempValue(vt.ProcedureType)

      Some(new InvokableProcedureCell(boxedProcTemp))
    }
  }
  
  def toSchemeTempValue(targetType : vt.SchemeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    // Are our possible concrete types a subset of the target types?
    schemeType.satisfiesType(targetType) match {
      case Some(true) =>
        // Need to cast to the right type
        // We've confirmed that no checking is needed because all of our possible types are equal to or supertypes of the
        // target type
        cellTempToSupertype(tempValue, tempType, targetType.cellType) 
    
      case None =>
        val errorMessage = errorMessageOpt getOrElse {
          RuntimeErrorMessage(
            name=s"subcastTo${targetType.cellType.llvmName.capitalize}Failed",
            text=s"Runtime cast to subtype '${targetType.schemeName}' failed"
          )
        }

        val targetCellType = targetType.cellType

        // Cast to the most specific cell type we can
        // RefineArgumentTypes will look for these steps when determining our argument type
        val castToCellTemp = if (targetCellType == tempType) {
          // Already of the correct cell type - we have work to do below
          tempValue
        }
        else {
          // We can cast to a more specific cell type
          val castTemp = new ps.TempValue(tempValue.isGcManaged)
          val possibleCellTypes = schemeType.possibleCellRepresentations
          plan.steps += ps.CastCellToSubtypeChecked(castTemp, worldPtr, tempValue, targetCellType, errorMessage, possibleCellTypes)

          castTemp
        }

        // Find the type after the cell cast
        val castToCellType = schemeType & vt.SchemeType.fromCellType(targetCellType)

        if (castToCellType.satisfiesType(targetType) == Some(true)) {
          // Our cell cast completely satisfied our type
          castToCellTemp
        }
        else {
          // We have further type checking to do
          val isTypePred = PlanTypeCheck(
            valueTemp=tempValue,
            valueType=castToCellType,
            testingType=targetType,
            isTypeBuilder={ isTypePlan =>
              val truePredTemp = new ps.TempValue(tempValue.isGcManaged)
              isTypePlan.steps += ps.CreateNativeInteger(truePredTemp, 1, vt.Predicate.bits)
              truePredTemp
            },
            isNotTypeBuilder={ isNotTypePlan =>
              val falsePredTemp = new ps.TempValue(tempValue.isGcManaged)
              isNotTypePlan.steps += ps.CreateNativeInteger(falsePredTemp, 0, vt.Predicate.bits)
              falsePredTemp
            }
          )
              
          plan.steps += ps.AssertPredicate(worldPtr, isTypePred, errorMessage)

          val castTemp = new ps.TempValue(tempValue.isGcManaged)
          plan.steps += ps.CastCellToTypeUnchecked(castTemp, tempValue, targetType.cellType)
          castTemp
        }

      case Some(false) =>
        // Not possible
        impossibleConversion(s"Unable to convert ${typeDescription} to ${targetType.schemeName}") 
    }
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val boxedChar = toTempValue(vt.CharacterType)
      val unboxedTemp = ps.Temp(vt.UnicodeChar)
      plan.steps += ps.UnboxCharacter(unboxedTemp, boxedChar)

      unboxedTemp
      
    case intType : vt.IntType =>
      val boxedExactInt = toTempValue(vt.ExactIntegerType)
      val unboxedTemp = ps.Temp(vt.Int64)
      plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

      if (intType.bits == 64) {
        // Correct width
        unboxedTemp
      }
      else {
        val convTemp = ps.Temp(intType)

        // Convert to the right width
        plan.steps += ps.ConvertNativeInteger(convTemp, unboxedTemp, intType.bits, intType.signed) 
        convTemp
      }

    case fpType : vt.FpType =>
      val possiblyExactInt = schemeType.satisfiesType(vt.ExactIntegerType) != Some(false)
      val possiblyInexactRational = schemeType.satisfiesType(vt.InexactRationalType) != Some(false)

      if (!possiblyExactInt && !possiblyInexactRational) {
        // Not possible
        impossibleConversion(s"Unable to convert non-numeric ${typeDescription} to ${fpType.schemeName}")
      }
      else if (possiblyExactInt & !possiblyInexactRational) {
        // Unbox as exact int
        val boxedExactInt = toTempValue(vt.ExactIntegerType)
        val unboxedTemp = ps.Temp(vt.Int64)
        plan.steps += ps.UnboxExactInteger(unboxedTemp, boxedExactInt)

        // Convert to the wanted type
        val convTemp = ps.Temp(fpType)
        plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, unboxedTemp, true, fpType)

        convTemp
      }
      else if (!possiblyExactInt && possiblyInexactRational) {
        // Unbox as inexact rational
        val boxedInexactRational = toTempValue(vt.InexactRationalType)
        val unboxedTemp = ps.Temp(vt.Double)
        plan.steps += ps.UnboxInexactRational(unboxedTemp, boxedInexactRational)

        if (fpType == vt.Double) {
          // No conversion needed
          unboxedTemp
        }
        else {
          val convTemp = ps.Temp(fpType)

          plan.steps += ps.ConvertNativeFloat(convTemp, unboxedTemp, fpType)
          convTemp
        }
      }
      else {
        // We have to check types here and branch on the result
        val isExactIntPred = ps.Temp(vt.Predicate)

        val possibleCellTypes = schemeType.possibleCellRepresentations
        plan.steps += ps.TestCellType(isExactIntPred, tempValue, ct.ExactIntegerCell, possibleCellTypes)

        // Try again with constrained types
        // This will hit the branches above us
        val trueWriter = plan.forkPlan()
        val trueDynamicValue = new CellValue(vt.ExactIntegerType, tempType, tempValue)
        val trueTempValue = trueDynamicValue.toTempValue(fpType)(trueWriter, worldPtr)

        val falseWriter = plan.forkPlan()
        val refinedSchemeType = schemeType - vt.ExactIntegerType
        val falseDynamicValue = new CellValue(refinedSchemeType, tempType, tempValue)
        val falseTempValue = falseDynamicValue.toTempValue(fpType)(falseWriter, worldPtr)
      
        val phiTemp = ps.Temp(fpType)

        plan.steps += ps.CondBranch(phiTemp, isExactIntPred, 
          trueWriter.steps.toList, trueTempValue,
          falseWriter.steps.toList, falseTempValue) 

        phiTemp
      }

    case _ : vt.BoolLikeType =>
      throw new InternalCompilerErrorException("Attempt to directly convert to native boolean. Should be caught by toTruthyPredicate.")
  }
  
  protected def toRecordTempValue(recordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    // Convert ourselves to a record
    val recordTemp = toTempValue(vt.SchemeTypeAtom(ct.RecordCell))

    // Make sure we we're of the right class
    val errorMessage = errorMessageOpt getOrElse {
      RuntimeErrorMessage(
        name=s"recordClassIsNot${recordType.schemeName}",
        text=s"Runtime cast to record type '${recordType.schemeName}' failed"
      )
    }

    plan.steps += ps.AssertRecordLikeClass(worldPtr, recordTemp, recordType, errorMessage)

    recordTemp
  }

  def preferredRepresentation : vt.ValueType =
    schemeType
  
  def needsClosureRepresentation =
    true
}

