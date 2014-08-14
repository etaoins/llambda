#include "CharCell.h"

#include "alloc/allocator.h"

namespace lliby
{

CharCell* CharCell::createInstance(World &world, UnicodeChar unicodeChar)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) CharCell(unicodeChar);
}

}
