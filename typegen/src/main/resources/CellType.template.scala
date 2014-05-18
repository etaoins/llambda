package io.llambda.compiler.celltype
import io.llambda

import llambda.llvmir._
import llambda.compiler.InternalCompilerErrorException

sealed abstract class CastableValue {
  val irType : FirstClassType
  val llvmName : String
  
  def genPointerBitcast(block : IrBlockBuilder)(uncastValue : IrValue) : IrValue =
    if (uncastValue.irType == PointerType(irType)) {
      uncastValue
    }
    else {
      block.bitcastTo(llvmName + "Cast")(uncastValue, PointerType(irType))
    }
}

sealed abstract class CellType extends CastableValue with ${ROOT_CLASS_FIELDS_TRAIT} {
  val schemeName : String
  val supertype : Option[CellType]
  val directSubtypes : Set[CellType]

  def isTypeOrSubtypeOf(otherType : CellType) : Boolean = {
    if (otherType == this) {
      return true
    }

    supertype map (_.isTypeOrSubtypeOf(otherType)) getOrElse false
  }

  def isTypeOrSupertypeOf(otherType : CellType) : Boolean = {
    if (otherType == this) {
      return true
    }

    directSubtypes exists (_.isTypeOrSupertypeOf(otherType))
  }

  lazy val concreteTypes : Set[ConcreteCellType] = this match {
    case concreteType : ConcreteCellType => Set(concreteType)
    case abstractType => directSubtypes.flatMap(_.concreteTypes)
  }

  def genTypeCheck(startBlock : IrBlockBuilder)(valueCell : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {
    val datumValue = DatumCell.genPointerBitcast(startBlock)(valueCell)
    val typeId = DatumCell.genLoadFromTypeId(startBlock)(datumValue)

    // For every possible type ID jump to the success block
    val successEntries = concreteTypes.map { concreteType =>
      (concreteType.typeId -> successBlock)
    }

    startBlock.switch(${TYPE_TAG_FIELD_NAME}, failBlock, successEntries.toSeq : _*)
  }
}

sealed abstract class ConcreteCellType extends CellType {
  val ${TYPE_TAG_FIELD_NAME} : Long
}

sealed abstract class PreconstructedCellType extends ConcreteCellType

sealed abstract class CellTypeVariant extends CastableValue

object CellType {
  val nextTbaaIndex = ${NEXT_TBAA_INDEX}L
}
