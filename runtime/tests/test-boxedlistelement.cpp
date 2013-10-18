#include "binding/BoxedListElement.h"
#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"
#include "binding/BoxedString.h"
#include "binding/ProperList.h"

#include "core/init.h"
#include "assertions.h"

namespace
{
	using namespace lliby;

	bool isProperList(const BoxedListElement *head)
	{
		return ProperList<BoxedDatum>(head).isValid();
	}

	std::uint32_t listLength(const BoxedListElement *head)
	{
		return ProperList<BoxedDatum>(head).length();
	}
}

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	BoxedString *valueA = BoxedString::fromUtf8CString("A");
	BoxedString *valueB = BoxedString::fromUtf8CString("B");
	BoxedString *valueC = BoxedString::fromUtf8CString("C");
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({});

		ASSERT_EQUAL(properList, BoxedEmptyList::instance());
		ASSERT_EQUAL(listLength(properList), 0);
	}
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({
			valueA
		});

		BoxedPair *onlyPair = datum_cast<BoxedPair>(properList);

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), BoxedEmptyList::instance());
		ASSERT_EQUAL(listLength(onlyPair), 1);
	}
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({
			valueA,
			valueB,
			valueC
		});

		BoxedPair *firstPair = datum_cast<BoxedPair>(properList);

		ASSERT_TRUE(firstPair != nullptr);
		ASSERT_EQUAL(firstPair->car(), valueA);
		ASSERT_EQUAL(listLength(firstPair), 3);

		BoxedPair *secondPair = datum_cast<BoxedPair>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(listLength(secondPair), 2);
		
		BoxedPair *thirdPair = datum_cast<BoxedPair>(secondPair->cdr());
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC);
		ASSERT_EQUAL(thirdPair->cdr(), BoxedEmptyList::instance());
		ASSERT_EQUAL(listLength(thirdPair), 1);
	}

	{
		BoxedPair *improperList = BoxedListElement::createImproperList({});
		ASSERT_TRUE(improperList == nullptr);
	}
	
	{
		BoxedPair *improperList = BoxedListElement::createImproperList({
			valueA
		});

		ASSERT_TRUE(improperList == nullptr);
	}
	
	{
		BoxedPair *onlyPair = BoxedListElement::createImproperList({
			valueA,
			valueB
		});

		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), valueB);
		ASSERT_FALSE(isProperList(onlyPair));
	}
	
	{
		BoxedPair *firstPair = BoxedListElement::createImproperList({
			valueA,
			valueB,
			valueC
		});

		ASSERT_EQUAL(firstPair->car(), valueA);
		ASSERT_FALSE(isProperList(firstPair));

		BoxedPair *secondPair = datum_cast<BoxedPair>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(secondPair->cdr(), valueC);
		ASSERT_FALSE(isProperList(secondPair));
	}
}
