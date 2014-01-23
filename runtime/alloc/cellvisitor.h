#ifndef _LLIBY_ALLOC_CELLVISITOR_H
#define _LLIBY_ALLOC_CELLVISITOR_H

#include <functional>

#include "binding/DatumCell.h"

namespace lliby
{
namespace alloc
{

/** 
 * Visits a cell by calling visitor
 *
 * @param rootCellRef  Pointer to the root cell pointer to visit. The visitor may modify this pointer.
 * @param visitor      Function to visit the cell. It is passed a pointer to the actual cell pointer to allow the cell
 *                     to be relocated. If the function returns true the child cells of the passed cell will be visited
 *                     next.
 */
void visitCell(DatumCell **rootCellRef, std::function<bool(DatumCell **)> &visitor);

#ifndef NDEBUG
/**
 * Dumps all reachable cells to std::cout
 *
 * This is for debug purposes only
 */
void dumpReachableFrom(DatumCell *rootCell, bool dumpGlobalConstants = true);
#endif

}
}

#endif

