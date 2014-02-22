#include <functional>

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharacterCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/VectorCell.h"
#include "binding/RecordCell.h"

#include "unicode/UnicodeChar.h"

#include "alloc/allocator.h"
#include "alloc/cellvisitor.h"
#include "alloc/StrongRef.h"
#include "alloc/WeakRef.h"
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

		alloc::forceCollection();

		// Now it should be unallocated
		ASSERT_TRUE(weakCell.isNull());
	}
	
	{
		// Use the WeakRef as a safe way to test if the strong ref worked
		alloc::WeakRef<T> weakCell(world, constructor());
		alloc::StrongRef<T> strongCell(world, weakCell);

		// This should be allocated now
		ASSERT_FALSE(weakCell.isNull());
		
		alloc::forceCollection();

		// This should still be allocated
		ASSERT_FALSE(weakCell.isNull());
	}
}

void testPairGc(World &world)
{
	// Make sure non-recursive GC works at a minimum
	testNonRecursiveGc<PairCell>(world, [] ()
	{
		return new PairCell(EmptyList, EmptyList);
	});

	// These need to be strong while allocated so value A/B don't disappear when later values are allocated 
	// However, they need to be weak after to test how strong references affect their collection
	alloc::StrongRef<StringCell> valueAStrong(world, StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> valueBStrong(world, StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> valueCStrong(world, StringCell::fromUtf8CString(""));
	
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
		alloc::StrongRef<PairCell> rootingRef(world, pairA);

		alloc::forceCollection();

		ASSERT_FALSE(pairA.isNull());
		ASSERT_FALSE(valueA.isNull());
		
		ASSERT_FALSE(pairB.isNull());
		ASSERT_FALSE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}
	
	{
		// Root the middle of the list
		alloc::StrongRef<PairCell> rootingRef(world, pairB);

		alloc::forceCollection();

		ASSERT_TRUE(pairA.isNull());
		ASSERT_TRUE(valueA.isNull());
		
		ASSERT_FALSE(pairB.isNull());
		ASSERT_FALSE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}

	{
		// Root the end of the list
		alloc::StrongRef<PairCell> rootingRef(world, pairC);

		alloc::forceCollection();

		ASSERT_TRUE(pairA.isNull());
		ASSERT_TRUE(valueA.isNull());
		
		ASSERT_TRUE(pairB.isNull());
		ASSERT_TRUE(valueB.isNull());
		
		ASSERT_FALSE(pairC.isNull());
		ASSERT_FALSE(valueC.isNull());
	}

	{
		// Root nothihg
		alloc::forceCollection();
		
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
	testNonRecursiveGc<VectorCell>(world, [] ()
	{
		return new VectorCell(nullptr, 0);
	});
	
	alloc::StrongRef<StringCell> value0Strong(world, StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> value1Strong(world, StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> value2Strong(world, StringCell::fromUtf8CString(""));
	
	alloc::StrongRef<VectorCell> testVec(world, VectorCell::fromFill(world, 3));
	
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
		alloc::forceCollection();

		ASSERT_FALSE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value0
		testVec->setElementAt(0, EmptyList);

		alloc::forceCollection();

		ASSERT_TRUE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value1
		testVec->setElementAt(1, EmptyList);

		alloc::forceCollection();

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
		ASSERT_FALSE(value2.isNull());
	}
	
	{
		// Remove value2
		testVec->setElementAt(2, EmptyList);

		alloc::forceCollection();

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
		ASSERT_TRUE(value2.isNull());
	}
}

void testRecordLikeGc(World &world)
{
	struct CustomRecordLikeData
	{
		DatumCell *cell0;
		std::uint32_t native;
		DatumCell *cell1;
	};

	// Register the record class
	const std::uint32_t testClass = RecordLikeCell::registerRuntimeRecordClass({
			offsetof(CustomRecordLikeData, cell0),
			offsetof(CustomRecordLikeData, cell1)});

	// Make some test values
	alloc::StrongRef<StringCell> value0Strong(world, StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> value1Strong(world, StringCell::fromUtf8CString(""));
	
	// Create the record-like
	alloc::StrongRef<RecordCell> testRecord(world, new RecordCell(testClass, false, nullptr));

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
		alloc::forceCollection();

		ASSERT_FALSE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
	}
	
	{
		// Unset the first value
		data->cell0 = EmptyList;
		alloc::forceCollection();

		ASSERT_TRUE(value0.isNull());
		ASSERT_FALSE(value1.isNull());
	}
	
	{
		// Unset the second value
		data->cell1 = EmptyList;
		alloc::forceCollection();

		ASSERT_TRUE(value0.isNull());
		ASSERT_TRUE(value1.isNull());
	}
}

void testAll(World &world)
{
	// Test exact integers
	testNonRecursiveGc<ExactIntegerCell>(world, [] ()
	{
		return ExactIntegerCell::fromValue(5);
	});
	
	// Test inexact rationals
	testNonRecursiveGc<InexactRationalCell>(world, [] ()
	{
		return InexactRationalCell::fromValue(5.0);
	});
	
	// Test inline symbols
	testNonRecursiveGc<SymbolCell>(world, [&world] ()
	{
		return StringCell::fromUtf8CString(u8"")->toSymbol(world);
	});
	
	// Test heap symbols
	testNonRecursiveGc<SymbolCell>(world, [&world] ()
	{
		return StringCell::fromUtf8CString(u8"This is more than twelve bytes long")->toSymbol(world);
	});

	// Test inline strings
	testNonRecursiveGc<StringCell>(world, [] ()
	{
		return StringCell::fromUtf8CString(u8"");
	});
	
	// Test heap strings
	testNonRecursiveGc<StringCell>(world, [] ()
	{
		return StringCell::fromUtf8CString(u8"This is more than twelve bytes long");
	});
	
	// Test bytevectors
	testNonRecursiveGc<BytevectorCell>(world, [] ()
	{
		return new BytevectorCell(nullptr, 0);
	});
	
	// Test characters
	testNonRecursiveGc<CharacterCell>(world, [] ()
	{
		return new CharacterCell(UnicodeChar(0x61));
	});
	
	// Test pairs
	testPairGc(world);

	// Test vectors
	testVectorGc(world);

	// Test record-likes
	testRecordLikeGc(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);

	return 0;
}
