#include "binding/InexactRationalValue.h"
#include "core/init.h"
#include "assertions.h"

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	{
		InexactRationalValue *value = new InexactRationalValue(0.0);
		ASSERT_EQUAL(value->value(), 0.0);

		ASSERT_TRUE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalValue *value = new InexactRationalValue(256.5);
		ASSERT_EQUAL(value->value(), 256.5);
		
		ASSERT_FALSE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalValue *value = new InexactRationalValue(-500);
		ASSERT_EQUAL(value->value(), -500);
		
		ASSERT_TRUE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalValue *value = InexactRationalValue::NaN();

		ASSERT_TRUE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalValue *value = InexactRationalValue::positiveInfinity();

		ASSERT_FALSE(value->isNaN());
		ASSERT_TRUE(value->isInfinite());
		ASSERT_TRUE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalValue *value = InexactRationalValue::negativeInfinity();

		ASSERT_FALSE(value->isNaN());
		ASSERT_TRUE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_TRUE(value->isNegativeInfinity());
	}

	return 0;
}
