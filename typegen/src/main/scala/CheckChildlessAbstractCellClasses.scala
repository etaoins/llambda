package io.llambda.typegen

import annotation.tailrec

/** Verifies every abstract cell class has at least one child
  *
  * Childless abstract classes aren't useful and indicate a probable mistake
  * in the cell definition field.
  */
object CheckChildlessAbstractCellClasses {
  @tailrec
  def apply(cellClasses : List[CellClass]) : Unit = cellClasses match {
    // Any child classes must be defined after us
    // This reduces our algorithmic complexity a tad
    case cellClass :: tailClasses =>
      if (cellClass.instanceType == CellClass.Abstract) {
        if (!tailClasses.exists(_.optionalParent == Some(cellClass))) {
          throw new ChildlessAbstractCellClassException(cellClass)
        }
      }

      apply(tailClasses)

    case Nil => 
      // All done
  }
}

