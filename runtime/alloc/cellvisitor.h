#ifndef _LLIBY_ALLOC_CELLVISITOR_H
#define _LLIBY_ALLOC_CELLVISITOR_H

#include <functional>

#include "binding/AnyCell.h"
#include "alloc/CellRefRangeList.h"

namespace lliby
{

namespace dynamic
{
	class State;
	class Continuation;
}

namespace alloc
{

struct ShadowStackEntry;

/** 
 * Visits a cell by calling visitor
 *
 * @param rootCellRef  Pointer to the root cell pointer to visit. The visitor may modify this pointer.
 * @param visitor      Function to visit the cell. It is passed a pointer to the actual cell pointer to allow the cell
 *                     to be relocated. If the function returns true the child cells of the passed cell will be visited
 *                     next.
 */
void visitCell(AnyCell **rootCellRef, std::function<bool(AnyCell **)> &visitor);

/**
 * Visits a CellRefRangeList by calling a visitor
 */
void visitCellRefList(const CellRefRangeList &cellRefList, std::function<bool(AnyCell **)> &visitor);

/**
 * Visits a shadow stack
 */
void visitShadowStack(ShadowStackEntry *head, std::function<bool(AnyCell **)> &visitor);

/**
 * Visits a continuation
 */
void visitContinuation(dynamic::Continuation *continuation, std::function<bool(AnyCell **)> &visitor);

#ifndef NDEBUG
/**
 * Dumps all reachable cells to std::cout
 *
 * This is for debug purposes only
 */
void dumpReachableFrom(AnyCell *rootCell, bool dumpGlobalConstants = true);
#endif

}
}

#endif

