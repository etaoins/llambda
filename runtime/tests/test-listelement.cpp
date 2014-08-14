#include "binding/ListElementCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"

#include "alloc/cellref.h"

#include "core/init.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{

using namespace lliby;

bool isProperList(const ListElementCell *head)
{
	return ProperList<AnyCell>(head).isValid();
}

std::uint32_t listLength(const ListElementCell *head)
{
	return ProperList<AnyCell>(head).length();
}

void testAll(World &world)
{
	alloc::StringRef valueA(world, StringCell::fromUtf8CString(world, "A"));
	alloc::StringRef valueB(world, StringCell::fromUtf8CString(world, "B"));
	alloc::StringRef valueC(world, StringCell::fromUtf8CString(world, "C"));
	
	{
		ListElementCell *properList = ListElementCell::createProperList(world, {});

		ASSERT_EQUAL(properList, EmptyListCell::instance());
		ASSERT_EQUAL(listLength(properList), 0);
	}
	
	{
		ListElementCell *properList = ListElementCell::createProperList(world, {
			valueA
		});

		PairCell *onlyPair = cell_cast<PairCell>(properList);

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA.data());
		ASSERT_EQUAL(onlyPair->cdr(), EmptyListCell::instance());
		ASSERT_EQUAL(listLength(onlyPair), 1);
	}
	
	{
		ListElementCell *properList = ListElementCell::createProperList(world, {
			valueA,
			valueB,
			valueC
		});

		PairCell *firstPair = cell_cast<PairCell>(properList);

		ASSERT_TRUE(firstPair != nullptr);
		ASSERT_EQUAL(firstPair->car(), valueA.data());
		ASSERT_EQUAL(listLength(firstPair), 3);

		PairCell *secondPair = cell_cast<PairCell>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB.data());
		ASSERT_EQUAL(listLength(secondPair), 2);
		
		PairCell *thirdPair = cell_cast<PairCell>(secondPair->cdr());
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC.data());
		ASSERT_EQUAL(thirdPair->cdr(), EmptyListCell::instance());
		ASSERT_EQUAL(listLength(thirdPair), 1);
	}

	{
		AnyCell *improperList = ListElementCell::createList(world, {}, valueA);

		ASSERT_TRUE(improperList == valueA.data());
	}
	
	{
		AnyCell *improperList = ListElementCell::createList(world, {valueA}, valueB);

		auto *onlyPair = cell_cast<PairCell>(improperList);
		ASSERT_TRUE(onlyPair != nullptr);

		ASSERT_EQUAL(onlyPair->car(), valueA.data());
		ASSERT_EQUAL(onlyPair->cdr(), valueB.data());
		ASSERT_FALSE(isProperList(onlyPair));
	}
	
	{
		AnyCell *improperList = ListElementCell::createList(world, {
			valueA,
			valueB
		}, valueC);
		
		auto *firstPair = cell_cast<PairCell>(improperList);
		ASSERT_TRUE(firstPair != nullptr);

		ASSERT_EQUAL(firstPair->car(), valueA.data());
		ASSERT_FALSE(isProperList(firstPair));

		auto secondPair = cell_cast<PairCell>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB.data());
		ASSERT_EQUAL(secondPair->cdr(), valueC.data());
		ASSERT_FALSE(isProperList(secondPair));
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
