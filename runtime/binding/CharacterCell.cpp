#include "CharacterCell.h"

#include "alloc/allocator.h"

namespace lliby
{

CharacterCell* CharacterCell::createInstance(World &world, UnicodeChar unicodeChar)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) CharacterCell(unicodeChar);
}

}
