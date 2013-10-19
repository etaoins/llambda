#include "binding/BoxedString.h"
#include "binding/BoxedSymbol.h"
#include "binding/ProperList.h"

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
		BoxedListElement *emptyListHead = BoxedListElement::createProperList({});

		ProperList<BoxedDatum> properList(emptyListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}
	
	{
		BoxedListElement *stringListHead = BoxedListElement::createProperList({valueA, valueB, valueC});

		ProperList<BoxedDatum> properList(stringListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() != properList.end());
		ASSERT_EQUAL(properList.length(), 3);
		ASSERT_FALSE(properList.isEmpty());

		auto it = properList.begin();
		// Be tricky with the increment operators
		ASSERT_EQUAL(*(it++), valueA);

		ASSERT_EQUAL(*it, valueB);
		
		ASSERT_EQUAL(*(++it), valueC);

		it++;
		ASSERT_TRUE(it == properList.end());
	}
	
	{
		BoxedListElement *stringImproperHead = BoxedListElement::createImproperList({valueA, valueB, valueC});
		
		// Improper list
		ProperList<BoxedString> properList(stringImproperHead);

		ASSERT_FALSE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}

}
