#include "core/init.h"
#include "core/World.h"

#include "binding/InexactRationalCell.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;


void testAll(World &world)
{
	{
		InexactRationalCell *value = InexactRationalCell::fromValue(0.0);
		ASSERT_EQUAL(value->value(), 0.0);

		ASSERT_TRUE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalCell *value = InexactRationalCell::fromValue(256.5);
		ASSERT_EQUAL(value->value(), 256.5);
		
		ASSERT_FALSE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalCell *value = InexactRationalCell::fromValue(-500);
		ASSERT_EQUAL(value->value(), -500);
		
		ASSERT_TRUE(value->isInteger());
		ASSERT_FALSE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalCell *value = InexactRationalCell::NaN();

		ASSERT_TRUE(value->isNaN());
		ASSERT_FALSE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalCell *value = InexactRationalCell::positiveInfinity();

		ASSERT_FALSE(value->isNaN());
		ASSERT_TRUE(value->isInfinite());
		ASSERT_TRUE(value->isPositiveInfinity());
		ASSERT_FALSE(value->isNegativeInfinity());
	}
	
	{
		InexactRationalCell *value = InexactRationalCell::negativeInfinity();

		ASSERT_FALSE(value->isNaN());
		ASSERT_TRUE(value->isInfinite());
		ASSERT_FALSE(value->isPositiveInfinity());
		ASSERT_TRUE(value->isNegativeInfinity());
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);

	return 0;
}
