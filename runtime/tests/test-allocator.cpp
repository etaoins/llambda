#include <functional>

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "binding/ProperList.h"
#include "binding/BooleanCell.h"
#include "binding/EmptyListCell.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

namespace
{
using namespace lliby;

EmptyListCell *EmptyList = EmptyListCell::instance();

void createListOfSize(World &world, std::size_t cellCount)
{
	std::vector<AnyCell*> falseCells;
	falseCells.resize(cellCount);

	for(std::size_t i = 0; i < cellCount; i++)
	{
		falseCells[i] = const_cast<BooleanCell*>(BooleanCell::falseInstance());
	}

	ProperList<AnyCell> *properList = ProperList<AnyCell>::create(world, falseCells);

	// This iterates over the whole list so this should make sure the allocation succeeded
	ASSERT_EQUAL(properList->size(), cellCount);
}

void testHugeRangeAlloc(World &world)
{
	// This ensures we can allocate large chunks of GCed memory at once
	const std::size_t cellCount = 1024 * 1024;
	createListOfSize(world, cellCount);

	// Force GC to ensure the collector doesn't crash
	alloc::forceCollection(world);
}

void testLargeNumberOfAllocations(World &world)
{
	// Allocate a series of ever increasingly large lists
	std::size_t allocationSize = 257;
	std::size_t allocationsLeft = 128;

	while(allocationsLeft--)
	{
		createListOfSize(world, allocationSize);
		allocationSize *= 1.05;
	}

	// Force GC to ensure the collector doesn't crash
	alloc::forceCollection(world);
}

void testAll(World &world)
{
	// Test large allocations
	testHugeRangeAlloc(world);

	// Test large number of allocations
	testLargeNumberOfAllocations(world);
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
