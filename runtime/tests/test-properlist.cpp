#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/ProperList.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "alloc/StrongRef.h"

namespace
{

using namespace lliby;

void testAll(World &world)
{
	alloc::StrongRef<StringCell> valueA(world, StringCell::fromUtf8CString("A"));
	alloc::StrongRef<StringCell> valueB(world, StringCell::fromUtf8CString("B"));
	alloc::StrongRef<StringCell> valueC(world, StringCell::fromUtf8CString("C"));
	
	{
		ListElementCell *emptyListHead = ListElementCell::createProperList(world, {});

		ProperList<DatumCell> properList(emptyListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}
	
	{
		ListElementCell *stringListHead = ListElementCell::createProperList(world, {valueA, valueB, valueC});

		ProperList<DatumCell> properList(stringListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() != properList.end());
		ASSERT_EQUAL(properList.length(), 3);
		ASSERT_FALSE(properList.isEmpty());

		auto it = properList.begin();
		// Be tricky with the increment operators
		ASSERT_EQUAL(*(it++), valueA.data());

		ASSERT_EQUAL(*it, valueB.data());
		
		ASSERT_EQUAL(*(++it), valueC.data());

		it++;
		ASSERT_TRUE(it == properList.end());
	}
	
	{
		DatumCell *improperList = ListElementCell::createList(world, {valueA, valueB}, valueC);
		
		auto stringImproperHead = datum_cast<ListElementCell>(improperList);
		ASSERT_TRUE(stringImproperHead != nullptr);

		// Improper list
		ProperList<StringCell> properList(stringImproperHead);

		ASSERT_FALSE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
