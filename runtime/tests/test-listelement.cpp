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
	return cell_cast<ProperList<AnyCell>>(head) != nullptr;
}

void testAll(World &world)
{
	alloc::StringRef valueA(world, StringCell::fromUtf8StdString(world, "A"));
	alloc::StringRef valueB(world, StringCell::fromUtf8StdString(world, "B"));
	alloc::StringRef valueC(world, StringCell::fromUtf8StdString(world, "C"));

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
	llcore_init(argc, argv);

	lliby::World::launchWorld(&testAll);
}
