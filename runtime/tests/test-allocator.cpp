#include <functional>

#include "core/init.h"
#include "assertions.h"

#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharacterCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/VectorCell.h"

#include "unicode/UnicodeChar.h"

#include "alloc/allocator.h"
#include "alloc/cellvisitor.h"
#include "alloc/StrongRef.h"
#include "alloc/WeakRef.h"
#include "alloc/RangeAlloc.h"

namespace
{
using namespace lliby;
	
EmptyListCell *EmptyList = const_cast<EmptyListCell*>(EmptyListCell::instance());

template<class T>
void testNonRecursiveGc(T* (*constructor)())
{
	{
		alloc::WeakRef<T> weakCell((*constructor)());

		// This should be allocated now
		ASSERT_FALSE(weakCell.isNull());

		alloc::forceCollection();

		// Now it should be unallocated
		ASSERT_TRUE(weakCell.isNull());
	}
	
	{
		// Use the WeakRef as a safe way to test if the strong ref worked
		alloc::WeakRef<T> weakCell((*constructor)());
		alloc::StrongRef<T> strongCell(weakCell);

		// This should be allocated now
		ASSERT_FALSE(weakCell.isNull());
		
		alloc::forceCollection();

		// This should still be allocated
		ASSERT_FALSE(weakCell.isNull());
	}
}

void testPairGc()
{
	// Make sure non-recursive GC works at a minimum
	testNonRecursiveGc<PairCell>([] ()
	{
		return new PairCell(EmptyList, EmptyList);
	});

	// These need to be strong while allocated so value A/B don't disappear when later values are allocated 
	// However, they need to be weak after to test how strong references affect their collection
	alloc::StrongRef<StringCell> valueAStrong(StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> valueBStrong(StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> valueCStrong(StringCell::fromUtf8CString(""));
	
	alloc::RangeAlloc allocation(alloc::allocateRange(3)); 
	auto allocIt = allocation.begin();

	// Make a simple proper list manually
	alloc::WeakRef<PairCell> pairC(new (*allocIt++) PairCell(valueCStrong, EmptyList));
	alloc::WeakRef<PairCell> pairB(new (*allocIt++) PairCell(valueBStrong, pairC));
	alloc::WeakRef<PairCell> pairA(new (*allocIt++) PairCell(valueAStrong, pairB));
	
	// Demote these to weak references
	alloc::WeakRef<StringCell> valueA = valueAStrong.data();
	valueAStrong = nullptr;

	alloc::WeakRef<StringCell> valueB = valueBStrong.data();
	valueBStrong = nullptr;

	alloc::WeakRef<StringCell> valueC = valueCStrong.data();
	valueCStrong = nullptr;

	{
		// Root the head of the list
		alloc::StrongRef<PairCell> rootingRef(pairA);

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
		alloc::StrongRef<PairCell> rootingRef(pairB);

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
		alloc::StrongRef<PairCell> rootingRef(pairC);

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

void testVectorGc()
{
	// Make sure non-recursive GC works at a minimum
	testNonRecursiveGc<VectorCell>([] ()
	{
		return new VectorCell(nullptr, 0);
	});
	
	alloc::StrongRef<StringCell> value0Strong(StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> value1Strong(StringCell::fromUtf8CString(""));
	alloc::StrongRef<StringCell> value2Strong(StringCell::fromUtf8CString(""));
	
	alloc::StrongRef<VectorCell> testVec(VectorCell::fromFill(3));
	
	alloc::WeakRef<StringCell> value0(value0Strong.data());
	value0Strong = nullptr;

	alloc::WeakRef<StringCell> value1(value1Strong.data());
	value1Strong = nullptr;

	alloc::WeakRef<StringCell> value2(value2Strong.data());
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

}

int main(int argc, char *argv[])
{
	lliby_init();

	// Test exact integers
	testNonRecursiveGc<ExactIntegerCell>([] ()
	{
		return new ExactIntegerCell(5);
	});
	
	// Test inexact rationals
	testNonRecursiveGc<InexactRationalCell>([] ()
	{
		return new InexactRationalCell(5.0);
	});
	
	// Test inline symbols
	testNonRecursiveGc<SymbolCell>([] ()
	{
		return StringCell::fromUtf8CString(u8"")->toSymbol();
	});
	
	// Test heap symbols
	testNonRecursiveGc<SymbolCell>([] ()
	{
		return StringCell::fromUtf8CString(u8"This is more than twelve bytes long")->toSymbol();
	});

	// Test inline strings
	testNonRecursiveGc<StringCell>([] ()
	{
		return StringCell::fromUtf8CString(u8"");
	});
	
	// Test heap strings
	testNonRecursiveGc<StringCell>([] ()
	{
		return StringCell::fromUtf8CString(u8"This is more than twelve bytes long");
	});
	
	// Test bytevectors
	testNonRecursiveGc<BytevectorCell>([] ()
	{
		return new BytevectorCell(nullptr, 0);
	});
	
	// Test characters
	testNonRecursiveGc<CharacterCell>([] ()
	{
		return new CharacterCell(UnicodeChar(0x61));
	});
	
	// Test pairs
	testPairGc();

	// Test vectors
	testVectorGc();

	return 0;
}
