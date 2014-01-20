#include "binding/ListElementCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"

#include "alloc/StrongRef.h"
#include "alloc/WeakRef.h"

#include "core/init.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
	using namespace lliby;

	bool isProperList(const ListElementCell *head)
	{
		return ProperList<DatumCell>(head).isValid();
	}

	std::uint32_t listLength(const ListElementCell *head)
	{
		return ProperList<DatumCell>(head).length();
	}
}

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	alloc::StrongRef<StringCell> valueA(StringCell::fromUtf8CString("A"));
	alloc::StrongRef<StringCell> valueB(StringCell::fromUtf8CString("B"));
	alloc::StrongRef<StringCell> valueC(StringCell::fromUtf8CString("C"));
	
	{
		ListElementCell *properList = ListElementCell::createProperList({});

		ASSERT_EQUAL(properList, EmptyListCell::instance());
		ASSERT_EQUAL(listLength(properList), 0);
	}
	
	{
		ListElementCell *properList = ListElementCell::createProperList({
			valueA
		});

		PairCell *onlyPair = datum_cast<PairCell>(properList);

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA.data());
		ASSERT_EQUAL(onlyPair->cdr(), EmptyListCell::instance());
		ASSERT_EQUAL(listLength(onlyPair), 1);
	}
	
	{
		ListElementCell *properList = ListElementCell::createProperList({
			valueA,
			valueB,
			valueC
		});

		PairCell *firstPair = datum_cast<PairCell>(properList);

		ASSERT_TRUE(firstPair != nullptr);
		ASSERT_EQUAL(firstPair->car(), valueA.data());
		ASSERT_EQUAL(listLength(firstPair), 3);

		PairCell *secondPair = datum_cast<PairCell>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB.data());
		ASSERT_EQUAL(listLength(secondPair), 2);
		
		PairCell *thirdPair = datum_cast<PairCell>(secondPair->cdr());
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC.data());
		ASSERT_EQUAL(thirdPair->cdr(), EmptyListCell::instance());
		ASSERT_EQUAL(listLength(thirdPair), 1);
	}

	{
		DatumCell *improperList = ListElementCell::createList({}, valueA);

		ASSERT_TRUE(improperList == valueA.data());
	}
	
	{
		DatumCell *improperList = ListElementCell::createList({valueA}, valueB);

		auto *onlyPair = datum_cast<PairCell>(improperList);
		ASSERT_TRUE(onlyPair != nullptr);

		ASSERT_EQUAL(onlyPair->car(), valueA.data());
		ASSERT_EQUAL(onlyPair->cdr(), valueB.data());
		ASSERT_FALSE(isProperList(onlyPair));
	}
	
	{
		DatumCell *improperList = ListElementCell::createList({
			valueA,
			valueB
		}, valueC);
		
		auto *firstPair = datum_cast<PairCell>(improperList);
		ASSERT_TRUE(firstPair != nullptr);

		ASSERT_EQUAL(firstPair->car(), valueA.data());
		ASSERT_FALSE(isProperList(firstPair));

		auto secondPair = datum_cast<PairCell>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB.data());
		ASSERT_EQUAL(secondPair->cdr(), valueC.data());
		ASSERT_FALSE(isProperList(secondPair));
	}
}
