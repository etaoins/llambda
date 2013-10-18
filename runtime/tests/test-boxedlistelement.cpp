#include "binding/BoxedListElement.h"
#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"
#include "binding/BoxedString.h"

#include "core/init.h"
#include "assertions.h"

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
		ASSERT_EQUAL(properList->listLength(), 0);
	}
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({
			valueA
		});

		BoxedPair *onlyPair = datum_cast<BoxedPair>(properList);

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), BoxedEmptyList::instance());
		ASSERT_EQUAL(onlyPair->listLength(), 1);
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
		ASSERT_EQUAL(firstPair->listLength(), 3);

		BoxedPair *secondPair = datum_cast<BoxedPair>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(secondPair->listLength(), 2);
		
		BoxedPair *thirdPair = datum_cast<BoxedPair>(secondPair->cdr());
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC);
		ASSERT_EQUAL(thirdPair->cdr(), BoxedEmptyList::instance());
		ASSERT_EQUAL(thirdPair->listLength(), 1);
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
		ASSERT_EQUAL(onlyPair->listLength(), BoxedListElement::InvalidListLength);
	}
	
	{
		BoxedPair *firstPair = BoxedListElement::createImproperList({
			valueA,
			valueB,
			valueC
		});

		ASSERT_EQUAL(firstPair->car(), valueA);
		ASSERT_EQUAL(firstPair->listLength(), BoxedListElement::InvalidListLength);

		BoxedPair *secondPair = datum_cast<BoxedPair>(firstPair->cdr());
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(secondPair->cdr(), valueC);
		ASSERT_EQUAL(secondPair->listLength(), BoxedListElement::InvalidListLength);
	}
}
