#ifndef _LLIBY_BINDING_LISTELEMENTCELL_H
#define _LLIBY_BINDING_LISTELEMENTCELL_H

#include "AnyCell.h"

#include <vector>

namespace lliby
{

class World;

template<class T>
class ProperList;

class ListElementCell : public AnyCell
{
#include "generated/ListElementCellMembers.h"
public:
	/**
	 * Creates a new proper or improper list
	 *
	 * @param  world    Pointer to the current world
	 * @param  element  Vector of elements to include in the list in their intended order. If allocating the pairs
	 *                  for the new list triggers garabge collection this vector will be updated to point to the new
	 *                  location of the cells.
	 * @param  tail     Tail element of the list. If this is EmptyListCell::instance() this will construct a proper
	 *                  list; otherwise, the list will improper
	 */
	static AnyCell *createList(World &world, std::vector<AnyCell*> &elements, AnyCell *tail);

	static AnyCell *createList(World &world, std::initializer_list<AnyCell*> elementsList, AnyCell *tail)
	{
		std::vector<AnyCell*> elements(elementsList);
		return createList(world, elements, tail);
	}

	static AnyCell *createList(World &world, const std::vector<AnyCell*> &elements, AnyCell *tail)
	{
		std::vector<AnyCell*> elementsCopy(elements);
		return createList(world, elementsCopy, tail);
	}

protected:
	explicit ListElementCell(CellTypeId typeId) :
		AnyCell(typeId)
	{
	}

	ListElementCell(CellTypeId typeId, GarbageState gcState) :
		AnyCell(typeId, gcState)
	{
	}
};

}

#endif
