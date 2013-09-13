#include "binding/PairValue.h"
#include "binding/EmptyListValue.h"
#include "binding/StringValue.h"

#include "core/init.h"
#include "assertions.h"

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	StringValue *valueA = StringValue::fromUtf8CString("A");
	StringValue *valueB = StringValue::fromUtf8CString("B");
	StringValue *valueC = StringValue::fromUtf8CString("C");
	
	{
		BoxedDatum *properList = PairValue::createProperList({});

		ASSERT_EQUAL(properList, EmptyListValue::instance());
	}
	
	{
		BoxedDatum *properList = PairValue::createProperList({
			valueA
		});

		PairValue *onlyPair = properList->asPairValue();

		ASSERT_TRUE(onlyPair != nullptr);
		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), EmptyListValue::instance());
	}
	
	{
		BoxedDatum *properList = PairValue::createProperList({
			valueA,
			valueB,
			valueC
		});

		PairValue *firstPair = properList->asPairValue();

		ASSERT_TRUE(firstPair != nullptr);
		ASSERT_EQUAL(firstPair->car(), valueA);

		PairValue *secondPair = firstPair->cdr()->asPairValue();
		ASSERT_TRUE(secondPair != nullptr);
		ASSERT_EQUAL(secondPair->car(), valueB);
		
		PairValue *thirdPair = secondPair->cdr()->asPairValue();
		ASSERT_TRUE(thirdPair != nullptr);
		ASSERT_EQUAL(thirdPair->car(), valueC);
		ASSERT_EQUAL(thirdPair->cdr(), EmptyListValue::instance());
	}

	{
		PairValue *improperList = PairValue::createImproperList({});
		ASSERT_TRUE(improperList == nullptr);
	}
	
	{
		PairValue *improperList = PairValue::createImproperList({
			valueA
		});

		ASSERT_TRUE(improperList == nullptr);
	}
	
	{
		PairValue *onlyPair = PairValue::createImproperList({
			valueA,
			valueB
		});

		ASSERT_EQUAL(onlyPair->car(), valueA);
		ASSERT_EQUAL(onlyPair->cdr(), valueB);
	}
	
	{
		PairValue *firstPair = PairValue::createImproperList({
			valueA,
			valueB,
			valueC
		});

		ASSERT_EQUAL(firstPair->car(), valueA);

		PairValue *secondPair = firstPair->cdr()->asPairValue();
		ASSERT_TRUE(secondPair != nullptr);
		
		ASSERT_EQUAL(secondPair->car(), valueB);
		ASSERT_EQUAL(secondPair->cdr(), valueC);
	}
}
