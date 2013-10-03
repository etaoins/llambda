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
	}
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({
			valueA
		});

		BoxedPair *onlyPair = properList->asBoxedPair();

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), BoxedEmptyList::instance());
	}
	
	{
		BoxedListElement *properList = BoxedListElement::createProperList({
			valueA,
			valueB,
			valueC
		});

		BoxedPair *firstPair = properList->asBoxedPair();

		ASSERT_TRUE(firstPair != nullptr);
		ASSERT_EQUAL(firstPair->car(), valueA);

		BoxedPair *secondPair = firstPair->cdr()->asBoxedPair();
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB);
		
		BoxedPair *thirdPair = secondPair->cdr()->asBoxedPair();
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC);
		ASSERT_EQUAL(thirdPair->cdr(), BoxedEmptyList::instance());
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
	}
	
	{
		BoxedPair *firstPair = BoxedListElement::createImproperList({
			valueA,
			valueB,
			valueC
		});

		ASSERT_EQUAL(firstPair->car(), valueA);

		BoxedPair *secondPair = firstPair->cdr()->asBoxedPair();
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(secondPair->cdr(), valueC);
	}
}
