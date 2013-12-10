package llambda.valuetype

/** Procedure cell with no closure 
  *
  * Closures have no requirement to have disjoint types. This means every empty
  * closure can share the same record cell type */
object EmptyClosureType extends ClosureType("empty-closure", Nil)
