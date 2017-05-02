package io.llambda.compiler.celltype
import io.llambda

import llambda.llvmir._
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.codegen.GlobalDefines


sealed abstract class CastableValue {
  val irType: FirstClassType
  val llvmName: String

  def genPointerBitcast(block: IrBlockBuilder)(uncastValue: IrValue): IrValue =
    if (uncastValue.irType == PointerType(irType)) {
      uncastValue
    }
    else {
      block.bitcastTo(llvmName + "Cast")(uncastValue, PointerType(irType))
    }
}

sealed abstract class CellType extends CastableValue with ${ROOT_CLASS_FIELDS_TRAIT} {
  val schemeName: String
  val directSubtypes: Set[CellType]

  lazy val concreteTypes: Set[ConcreteCellType] = this match {
    case concreteType: ConcreteCellType => Set(concreteType)
    case abstractType => directSubtypes.flatMap(_.concreteTypes)
  }

  override def toString = schemeName
}

sealed abstract class ConcreteCellType extends CellType {
  val ${TYPE_TAG_FIELD_NAME}: Long
}

sealed abstract class PreconstructedCellType extends ConcreteCellType

sealed abstract class CellTypeVariant extends CastableValue

object CellType {
  val nextMetadataIndex = ${NEXT_METADATA_INDEX}L
}
