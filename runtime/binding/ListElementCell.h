#ifndef _LLIBY_BINDING_LISTELEMENTCELL_H
#define _LLIBY_BINDING_LISTELEMENTCELL_H

#include "DatumCell.h"

#include <vector>

namespace lliby
{

class ListElementCell : public DatumCell
{
#include "generated/ListElementCellMembers.h"
public:
	/**
	 * Convenience wrapper for createList(elements, EmptyListCell::instance())
	 *
	 * See createList() for more information
	 */
	static ListElementCell *createProperList(std::vector<DatumCell*> &elements);
	
	static ListElementCell *createProperList(const std::vector<DatumCell*> &elements)
	{
		std::vector<DatumCell*> elementsCopy(elements);
		return createProperList(elementsCopy);
	}

	/**
	 * Creates a new proper or improper list
	 *
	 * @param  element  Vector of elements to include in the list in their intended order. If allocating the pairs
	 *                  for the new list triggers garabge collection this vector will be updated to point to the new
	 *                  location of the cells.
	 * @param  tail     Tail element of the list. If this is EmptyListCell::instance() this will construct a proper
	 *                  list; otherwise, the list will improper
	 */
	static DatumCell *createList(std::vector<DatumCell*> &elements, DatumCell *tail);

	static DatumCell *createList(const std::vector<DatumCell*> &elements, DatumCell *tail)
	{
		std::vector<DatumCell*> elementsCopy(elements);
		return createList(elementsCopy, tail);
	}

protected:
	explicit ListElementCell(CellTypeId typeId) :
		DatumCell(typeId)
	{
	}

	ListElementCell(CellTypeId typeId, GarbageState gcState) :
		DatumCell(typeId, gcState)
	{
	}
};

}

#endif
