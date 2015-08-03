#ifndef _LLIBY_BINDING_HASHMAPCELL_H
#define _LLIBY_BINDING_HASHMAPCELL_H

#include "AnyCell.h"

namespace lliby
{
class DatumHashTree;

class HashMapCell : public AnyCell
{
#include "generated/HashMapCellMembers.h"
public:
	explicit HashMapCell(DatumHashTree *datumHashTree) :
		AnyCell(CellTypeId::HashMap),
		m_datumHashTree(datumHashTree)
	{
	}

	/**
	 * Creates an empty HashMapCell
	 *
	 * It is not possible to directly create non-empty instance as DatumHashMaps can only be GC rooted using a
	 * HashMapCell. Creating a HashMapCell from a non-empty hash tree could enter the GC and invalidate all of the cell
	 * references in the existing tree.
	 */
	static HashMapCell* createEmptyInstance(World &world);

	void setDatumHashTree(DatumHashTree *tree)
	{
		m_datumHashTree = tree;
	}

	void finalizeHashMap();
};

}

#endif
