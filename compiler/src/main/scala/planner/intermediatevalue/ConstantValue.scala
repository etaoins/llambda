package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure}

sealed abstract class ConstantValue(cellType : ct.ConcreteCellType) extends IntermediateValue with UninvokableValue with NonRecordValue {
  val possibleTypes = Set(cellType)
    
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue

  def toCellTempValue(targetType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    if (targetType.isTypeOrSupertypeOf(cellType)) {
      val boxedTempValue = toConstantCellTempValue()
      
      if (targetType == cellType) {
        // Perfect!
        Some(boxedTempValue)
      }
      else {
        // Cast to supertype before returning
        val castTemp = new ps.TempValue
        plan.steps += ps.CastCellToTypeUnchecked(castTemp, boxedTempValue, targetType)

        Some(castTemp)
      }
    }
    else {
      // Impossible conversion
      None
    }
  
  def preferredRepresentation : vt.ValueType =
    vt.IntrinsicCellType(cellType)
  
  def closureRepresentation : Option[vt.ValueType] = 
    // Constants don't need to be captured
    None
}

sealed abstract class TrivialConstantValue[T, U <: ps.StoreConstantCell](cellType : ct.ConcreteCellType, value : T, stepConstructor : (ps.TempValue, T) => U) extends ConstantValue(cellType) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(value : String) extends TrivialConstantValue(ct.StringCell, value, ps.StoreStringCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = nativeType match {
    case vt.Utf8CString =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeUtf8String(constantTemp, value)
      Some(constantTemp)
    
    case _ => None
  }
}

class ConstantSymbolValue(value : String) extends TrivialConstantValue(ct.SymbolCell, value, ps.StoreSymbolCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Symbols have no NFI representation
    None
}

class ConstantExactIntegerValue(value : Long) extends TrivialConstantValue(ct.ExactIntegerCell, value, ps.StoreExactIntegerCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = nativeType match {
    case intType : vt.IntType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeInteger(constantTemp, value, intType.bits)
      Some(constantTemp)

    case fpType : vt.FpType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeFloat(constantTemp, value.toDouble, fpType)
      Some(constantTemp)

    case _ => None
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Int64
}

class ConstantInexactRationalValue(value : Double) extends TrivialConstantValue(ct.InexactRationalCell, value, ps.StoreInexactRationalCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = nativeType match {
    case fpType : vt.FpType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeFloat(constantTemp, value, fpType)
      Some(constantTemp)

    case _ => None
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Double
}

class ConstantCharacterValue(value : Char) extends TrivialConstantValue(ct.CharacterCell, value, ps.StoreCharacterCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = nativeType match {
    case vt.UnicodeChar =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeInteger(constantTemp, value, vt.UnicodeChar.bits)
      Some(constantTemp)

    case _ => None
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.UnicodeChar
}

class ConstantBooleanValue(value : Boolean) extends TrivialConstantValue(ct.BooleanCell, value, ps.StoreBooleanCell.apply) {
  private val intValue = if (value) 1 else 0

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = new ps.TempValue
    plan.steps += ps.StoreNativeInteger(predTemp, intValue, 1) 

    predTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    // toTruthyPredicate() will catch our conversion to bool
    None
  
  override def preferredRepresentation : vt.ValueType =
    vt.CBool
}

class ConstantBytevectorValue(value : Vector[Short]) extends TrivialConstantValue(ct.BytevectorCell, value, ps.StoreBytevectorCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Bytevectors can't be unboxed
    None
}

class ConstantPairValue(car : ConstantValue, cdr : ConstantValue) extends ConstantValue(ct.PairCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our car/cdr first
    val carTemp = car.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))
    val cdrTemp = cdr.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))

    plan.steps += ps.StorePairCell(constantTemp, carTemp, cdrTemp)

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Pairs cannot be unboxed
    None
}

class ConstantVectorValue(elements : Vector[ConstantValue]) extends ConstantValue(ct.VectorCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our elements
    val elementTemps = elements.map {
      _.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))
    }

    plan.steps += ps.StoreVectorCell(constantTemp, elementTemps)

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Pairs cannot be unboxed
    None
}

object EmptyListValue extends ConstantValue(ct.EmptyListCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += ps.StoreEmptyListCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    None
}

object UnspecificValue extends ConstantValue(ct.UnspecificCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += ps.StoreUnspecificCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    None
}

