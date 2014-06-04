package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.InternalCompilerErrorException
import llambda.llvmir._

/** Base class for value helper objects
  *
  * This interface is modelled after the CellType objects 
  */
abstract class StructureValue(typeName : String) {
  val irType = UserDefinedType(typeName)
  
  protected case class StructureField(name : String, index : Int, irType : FirstClassType, tbaaNode : Metadata)

  protected def genPointerToField(field : StructureField)(block : IrBlockBuilder)(structureValue : IrValue) : IrValue = {
    if (structureValue.irType != PointerType(irType)) {
      throw new InternalCompilerErrorException(s"Unexpected type for structure value. Passed ${structureValue.irType}, expected ${PointerType(irType)}")
    }

    block.getelementptr(field.name + "Ptr")(
      elementType=field.irType,
      basePointer=structureValue,
      indices=List(0, field.index).map(IntegerConstant(IntegerType(32), _)),
      inbounds=true
    )
  }
  
  protected def genStoreToField(field : StructureField)(block : IrBlockBuilder)(toStore : IrValue, structureValue : IrValue)  {
    val pointer = genPointerToField(field)(block)(structureValue)
    block.store(toStore, pointer, metadata=Map("tbaa" -> field.tbaaNode))
  }

  protected def genLoadFromField(field : StructureField)(block : IrBlockBuilder)(structureValue : IrValue) : IrValue = {
    val pointer = genPointerToField(field)(block)(structureValue)
    block.load(field.name)(pointer, metadata=Map("tbaa" -> field.tbaaNode))
  }
}

