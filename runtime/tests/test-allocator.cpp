#include <functional>

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/VectorCell.h"
#include "binding/RecordCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "unicode/UnicodeChar.h"

#include "alloc/allocator.h"
#include "alloc/cellvisitor.h"
#include "alloc/cellref.h"
#include "alloc/RangeAlloc.h"

namespace
{
using namespace lliby;
	
EmptyListCell *EmptyList = EmptyListCell::instance();

template<class T, class Function>
void testNonRecursiveGc(World &world, Function &&constructor)
{
	{
		alloc::WeakRef<T> weakCell(world, constructor());

		// This should be allocated now
		ASSERT_FALSE(weakCell.isNull());

		alloc::forceCollection(world);

		// Now it should be unallocated
		ASSERT_TRUE(weakCell.isNull());
	}
	
	{
		// Use the WeakRef as a safe way to test if the strong ref worked
		alloc::WeakRef<T> weakCell(world, constructor());
		alloc::StrongRef<T> strongCell(world, weakCell);

		// This should be allocated now
		ASSERT_FALSE(weakCell.isNull());
		
		alloc::forceCollection(world);

		// This should still be allocated
		ASSERT_FALSE(weakCell.isNull());
	}
}

void testPairGc(World &world)
{
	// Make sure non-recursive GC works at a minimum
	testNonRecursiveGc<PairCell>(world, [&world] ()
	{
		return PairCell::createInstance(world, EmptyList, EmptyList);
	});

	// These need to be strong while allocated so value A/B don't disappear when later values are allocated 
	// However, they need to be weak after to test how strong references affect their collection
	alloc::StringRef valueAStrong(world, StringCell::fromUtf8CString(world, ""));
	alloc::StringRef valueBStrong(world, StringCell::fromUtf8CString(world, ""));
	alloc::StringRef valueCStrong(world, StringCell::fromUtf8CString(world, ""));
	
	alloc::RangeAlloc allocation(alloc::allocateRange(world, 3)); 
	auto allocIt = allocation.begin();

	// Make a simple proper list manually
	alloc::WeakRef<PairCell> pairC(world, new (*allocIt++) PairCell(valueCStrong, EmptyList));
	alloc::WeakRef<PairCell> pairB(world, new (*allocIt++) PairCell(valueBStrong, pairC));
	alloc::WeakRef<PairCell> pairA(world, new (*allocIt++) PairCell(valueAStrong, pairB));
	
	// Demote these to weak references
	alloc::WeakRef<StringCell> valueA(world, valueAStrong.data());
	valueAStrong = nullptr;

	alloc::WeakRef<StringCell> valueB(world, valueBStrong.data());
	valueBStrong = nullptr;

	alloc::WeakRef<StringCell> valueC(world,  valueCStrong.data());
	valueCStrong = nullptr;

	{
		// Root the head of the list
		alloc::PairRef rootingRef(world, pairA);

		alloc::forceCollection(world);

		ASSERT_FALSE(pairA.isNull());
		ASSERT_FALSE(valueA.isNull());
		
		ASSERT_FALSE(pairB.isNull());
		ASSERT_FALSE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}
	
	{
		// Root the middle of the list
		alloc::PairRef rootingRef(world, pairB);

		alloc::forceCollection(world);

		ASSERT_TRUE(pairA.isNull());
		ASSERT_TRUE(valueA.isNull());
		
		ASSERT_FALSE(pairB.isNull());
		ASSERT_FALSE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}

	{
		// Root the end of the list
		alloc::PairRef rootingRef(world, pairC);

		alloc::forceCollection(world);

		ASSERT_TRUE(pairA.isNull());
		ASSERT_TRUE(valueA.isNull());
		
		ASSERT_TRUE(pairB.isNull());
		ASSERT_TRUE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}

	{
		// Root nothihg
		alloc::forceCollection(world);
		
		ASSERT_TRUE(pairA.isNull());
		ASSERT_TRUE(valueA.isNull());
		
		ASSERT_TRUE(pairB.isNull());
		ASSERT_TRUE(valueB.isNull());
		
		ASSERT_TRUE(pairC.isNull());
		ASSERT_TRUE(valueC.isNull());
	}
}

void testVectorGc(World &world)
{
	// Make sure non-recursive GC works at a minimum
	testNonRecursiveGc<VectorCell>(world, [&world] ()
	{
		return VectorCell::fromElements(world, nullptr, 0);
	});
	
	alloc::StringRef value0Strong(world, StringCell::fromUtf8CString(world, ""));
	alloc::StringRef value1Strong(world, StringCell::fromUtf8CString(world, ""));
	alloc::StringRef value2Strong(world, StringCell::fromUtf8CString(world, ""));
	
	alloc::VectorRef testVec(world, VectorCell::fromFill(world, 3));
	
	alloc::WeakRef<StringCell> value0(world, value0Strong.data());
	value0Strong = nullptr;

	alloc::WeakRef<StringCell> value1(world, value1Strong.data());
	value1Strong = nullptr;

	alloc::WeakRef<StringCell> value2(world, value2Strong.data());
	value2Strong = nullptr;

	testVec->setElementAt(0, value0);
	testVec->setElementAt(1, value1);
	testVec->setElementAt(2, value2);

	{
		alloc::forceCollection(world);

		ASSERT_FALSE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value0
		testVec->setElementAt(0, EmptyList);

		alloc::forceCollection(world);

		ASSERT_TRUE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value1
		testVec->setElementAt(1, EmptyList);

		alloc::forceCollection(world);

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value2
		testVec->setElementAt(2, EmptyList);

		alloc::forceCollection(world);

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
		ASSERT_TRUE(value2.isNull());
	}
}

void testRecordLikeGc(World &world)
{
	struct CustomRecordLikeData
	{
		AnyCell *cell0;
		std::uint32_t native;
		AnyCell *cell1;
	};

	// Register the record class
	const std::uint32_t testClass = RecordLikeCell::registerRuntimeRecordClass({
			offsetof(CustomRecordLikeData, cell0),
			offsetof(CustomRecordLikeData, cell1)});

	// Make some test values
	alloc::StringRef value0Strong(world, StringCell::fromUtf8CString(world, ""));
	alloc::StringRef value1Strong(world, StringCell::fromUtf8CString(world, ""));
	
	// Create the record-like
	alloc::RecordRef testRecord(world, RecordCell::createInstance(world, testClass, false, nullptr));

	// Set the data
	auto data = static_cast<CustomRecordLikeData*>(RecordLikeCell::allocateRecordData(sizeof(CustomRecordLikeData)));
	data->cell0 = value0Strong.data();
	data->cell1 = value1Strong.data();

	testRecord->setRecordData(data);
	
	alloc::WeakRef<StringCell> value0(world, value0Strong.data());
	value0Strong = nullptr;

	alloc::WeakRef<StringCell> value1(world, value1Strong.data());
	value1Strong = nullptr;

	{
		// Make sure the data stay after GC
		alloc::forceCollection(world);

		ASSERT_FALSE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
	}
	
	{
		// Unset the first value
		data->cell0 = EmptyList;
		alloc::forceCollection(world);

		ASSERT_TRUE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
	}
	
	{
		// Unset the second value
		data->cell1 = EmptyList;
		alloc::forceCollection(world);

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
	}
}

void createListOfSize(World &world, size_t cellCount)
{
	std::vector<AnyCell*> falseCells;
	falseCells.resize(cellCount);

	for(size_t i = 0; i < cellCount; i++)
	{
		falseCells[i] = const_cast<BooleanCell*>(BooleanCell::falseInstance());
	}

	ListElementCell *listHead = ListElementCell::createProperList(world, falseCells);

	// This iterates over the whole list so this should make sure the allocation succeeded
	ProperList<BooleanCell> properList(listHead);
	ASSERT_TRUE(properList.isValid());
	ASSERT_EQUAL(properList.length(), cellCount);
}

void testHugeRangeAlloc(World &world)
{
	// This ensures we can allocate large chunks of GCed memory at once
	const size_t cellCount = 1024 * 1024;
	createListOfSize(world, cellCount);
	
	// Force GC to ensure the collector doesn't crash
	alloc::forceCollection(world);
}

void testLargeNumberOfAllocations(World &world)
{
	// Allocate a series of ever increasingly large lists
	size_t allocationSize = 257;
	size_t allocationsLeft = 128; 

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
	// Test exact integers
	testNonRecursiveGc<ExactIntegerCell>(world, [&world] ()
	{
		return ExactIntegerCell::fromValue(world, 5);
	});
	
	// Test inexact rationals
	testNonRecursiveGc<FlonumCell>(world, [&world] ()
	{
		return FlonumCell::fromValue(world, 5.0);
	});
	
	// Test inline symbols
	testNonRecursiveGc<SymbolCell>(world, [&world] ()
	{
		return SymbolCell::fromString(world, StringCell::fromUtf8CString(world, u8""));
	});
	
	// Test heap symbols
	testNonRecursiveGc<SymbolCell>(world, [&world] ()
	{
		return SymbolCell::fromString(world, StringCell::fromUtf8CString(world, u8"This is more than twelve bytes long"));
	});

	// Test inline strings
	testNonRecursiveGc<StringCell>(world, [&world] ()
	{
		return StringCell::fromUtf8CString(world, u8"");
	});
	
	// Test heap strings
	testNonRecursiveGc<StringCell>(world, [&world] ()
	{
		return StringCell::fromUtf8CString(world, u8"This is more than twelve bytes long");
	});
	
	// Test bytevectors
	testNonRecursiveGc<BytevectorCell>(world, [&world] ()
	{
		return BytevectorCell::fromData(world, nullptr, 0);
	});
	
	// Test characters
	testNonRecursiveGc<CharCell>(world, [&world] ()
	{
		return CharCell::createInstance(world, UnicodeChar(0x61));
	});
	
	// Test pairs
	testPairGc(world);

	// Test vectors
	testVectorGc(world);

	// Test record-likes
	testRecordLikeGc(world);

	// Test large allocations
	testHugeRangeAlloc(world);

	// Test large number of allocations
	testLargeNumberOfAllocations(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);

	return 0;
}
