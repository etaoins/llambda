package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.llvmir._

object GenFeatureIdentifiers {
  def apply(module : IrModuleBuilder)(featureIdentifiers : Set[String]) {
    // Turn this in to a proper list
    val emptyList = GlobalDefines.emptyListIrValue

    // Sort so the output of (features) looks cleaner
    val sortedIdentifiers = featureIdentifiers.toList.sorted

    val listHeadUncast = sortedIdentifiers.zipWithIndex.foldRight(emptyList) { case ((featureIdent, index), nextListElement) =>
      val symbolIrConstant = GenConstant.genSymbolCell(module)(featureIdent)

      val pairCell = ct.PairCell.createConstant(
        memberTypeId=ct.SymbolCell.typeId,
        listLength=featureIdentifiers.size - index,
        car=BitcastToConstant(symbolIrConstant, PointerType(ct.DatumCell.irType)),
        cdr=BitcastToConstant(nextListElement, PointerType(ct.DatumCell.irType))
      )

      val pairName = module.nameSource.allocate("featureIdentifierPair")
      GenConstant.defineConstantData(module)(pairName, pairCell) 
    }

    // Build a function to return the features list
    // This allows the features list to be removed at runtime if its not needed
    val featuresFunction = new IrFunctionBuilder(
      result=IrFunction.Result(PointerType(ct.ListElementCell.irType)),
      namedArguments=Nil,
      name="__llambda_features",
      linkage=Linkage.Internal,
      attributes=Set(IrFunction.NoUnwind)
    )

    // Cast to a %listElement*
    val listHead = BitcastToConstant(listHeadUncast, PointerType(ct.ListElementCell.irType))
    // And return it
    featuresFunction.entryBlock.ret(listHead)
    
    module.defineFunction(featuresFunction)
  }
}
