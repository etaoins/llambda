#include "HashMapCell.h"

#include "alloc/allocator.h"
#include "hash/DatumHashTree.h"

namespace lliby
{

HashMapCell* HashMapCell::createEmptyInstance(World &world)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) HashMapCell(DatumHashTree::createEmpty());
}

void HashMapCell::finalizeHashMap()
{
	DatumHashTree::unref(m_datumHashTree);
}

}
