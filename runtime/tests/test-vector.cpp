#include "binding/VectorCell.h"
#include "binding/StringCell.h"
#include "binding/UnitCell.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

void testFromFill(World &world)
{
	{
		VectorCell *emptyVector  = VectorCell::fromFill(world, 0);

		ASSERT_EQUAL(emptyVector->length(), 0);
	}

	{
		VectorCell *unitVector  = VectorCell::fromFill(world, 5);

		ASSERT_EQUAL(unitVector->length(), 5);
		ASSERT_EQUAL(unitVector->elementAt(0), UnitCell::instance());
		ASSERT_EQUAL(unitVector->elementAt(4), UnitCell::instance());
		ASSERT_EQUAL(unitVector->elementAt(5), 0);
	}

	{
		StringCell *testString = StringCell::fromUtf8StdString(world, u8"Hello!");
		VectorCell *stringVector  = VectorCell::fromFill(world, 4, testString);

		ASSERT_EQUAL(stringVector->length(), 4);
		ASSERT_EQUAL(stringVector->elementAt(0), testString);
		ASSERT_EQUAL(stringVector->elementAt(3), testString);
		ASSERT_EQUAL(stringVector->elementAt(4), 0);
	}
}

void testFromAppended(World &world)
{
	StringCell *string1 = StringCell::fromUtf8StdString(world, u8"One");
	VectorCell *vector1 = VectorCell::fromFill(world, 3, string1);

	StringCell *string2 = StringCell::fromUtf8StdString(world, u8"Two");
	VectorCell *vector2 = VectorCell::fromFill(world, 1, string2);

	StringCell *string3 = StringCell::fromUtf8StdString(world, u8"Three");
	VectorCell *vector3 = VectorCell::fromFill(world, 3, string3);

	{
		VectorCell *emptyVector = VectorCell::fromAppended(world, {});

		ASSERT_EQUAL(emptyVector->length(), 0);
	}

	{
		VectorCell *appendedVector = VectorCell::fromAppended(world, {vector1});

		ASSERT_EQUAL(appendedVector->length(), 3);
		ASSERT_EQUAL(appendedVector->elementAt(0), string1);
		ASSERT_EQUAL(appendedVector->elementAt(2), string1);
		ASSERT_EQUAL(appendedVector->elementAt(3), 0);
	}

	{
		VectorCell *appendedVector = VectorCell::fromAppended(world, {vector1, vector2, vector3});

		ASSERT_EQUAL(appendedVector->length(), 7);
		ASSERT_EQUAL(appendedVector->elementAt(0), string1);
		ASSERT_EQUAL(appendedVector->elementAt(2), string1);
		ASSERT_EQUAL(appendedVector->elementAt(3), string2);
		ASSERT_EQUAL(appendedVector->elementAt(4), string3);
		ASSERT_EQUAL(appendedVector->elementAt(6), string3);
	}
}

void testSetElement(World &world)
{
	StringCell *testString = StringCell::fromUtf8StdString(world, u8"Test");
	VectorCell *testVector  = VectorCell::fromFill(world, 5);

	ASSERT_EQUAL(testVector->elementAt(0), UnitCell::instance());
	ASSERT_EQUAL(testVector->setElementAt(0, testString), true);
	ASSERT_EQUAL(testVector->elementAt(0), testString);

	ASSERT_EQUAL(testVector->elementAt(4), UnitCell::instance());
	ASSERT_EQUAL(testVector->setElementAt(4, testString), true);
	ASSERT_EQUAL(testVector->elementAt(4), testString);

	ASSERT_EQUAL(testVector->setElementAt(5, testString), false);
}

void testCopy(World &world)
{
	VectorCell *testVector = VectorCell::fromFill(world, 5);

	for(unsigned int i = 0; i < 5; i++)
	{
		StringCell *newString = StringCell::fromUtf8StdString(world, "TEST");
		testVector->setElementAt(i, newString);
	}

	{
		VectorCell *wholeCopy = testVector->copy(world);

		ASSERT_EQUAL(wholeCopy->length(), 5);

		ASSERT_EQUAL(memcmp(wholeCopy->elements(), testVector->elements(), 5 * sizeof(AnyCell*)), 0);
	}

	{
		VectorCell *explicitWholeCopy = testVector->copy(world, 0, 5);

		ASSERT_EQUAL(explicitWholeCopy->length(), 5);
		ASSERT_EQUAL(memcmp(explicitWholeCopy->elements(), testVector->elements(), 5 * sizeof(AnyCell*)), 0);
	}

	{
		VectorCell *firstTwoCopy = testVector->copy(world, 0, 2);

		ASSERT_EQUAL(firstTwoCopy->length(), 2);
		ASSERT_EQUAL(firstTwoCopy->elementAt(0), testVector->elementAt(0));
		ASSERT_EQUAL(firstTwoCopy->elementAt(1), testVector->elementAt(1));
	}

	{
		VectorCell *lastTwoCopy = testVector->copy(world, 3, 5);

		ASSERT_EQUAL(lastTwoCopy->length(), 2);
		ASSERT_EQUAL(lastTwoCopy->elementAt(0), testVector->elementAt(3));
		ASSERT_EQUAL(lastTwoCopy->elementAt(1), testVector->elementAt(4));
	}

	{
		VectorCell *emptyCopy = testVector->copy(world, 3, 3);

		ASSERT_EQUAL(emptyCopy->length(), 0);
	}
}

void testReplace(World &world)
{
	VectorCell *fromVector = VectorCell::fromFill(world, 5);

	for(unsigned int i = 0; i < 5; i++)
	{
		StringCell *newString = StringCell::fromUtf8StdString(world, "TEST");
		fromVector->setElementAt(i, newString);
	}

	AnyCell *destElements[5] = {nullptr};

	for(unsigned int i = 0; i < 5; i++)
	{
		destElements[i] = StringCell::fromUtf8StdString(world, "TEST");
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			StringCell *newString = StringCell::fromUtf8StdString(world, "TEST");
			toVector->setElementAt(i, newString);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector), true);
		ASSERT_EQUAL(toVector->length(), 5);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(toVector->elementAt(i), fromVector->elementAt(i));
		}
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			StringCell *newString = StringCell::fromUtf8StdString(world, "TEST");
			toVector->setElementAt(i, newString);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(toVector->elementAt(i), fromVector->elementAt(i));
		}
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, destElements[i]);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 2, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(toVector->elementAt(i), destElements[i]);
		}
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, destElements[i]);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		ASSERT_EQUAL(toVector->elementAt(0), fromVector->elementAt(0));
		ASSERT_EQUAL(toVector->elementAt(1), fromVector->elementAt(1));
		ASSERT_EQUAL(toVector->elementAt(2), destElements[2]);
		ASSERT_EQUAL(toVector->elementAt(3), destElements[3]);
		ASSERT_EQUAL(toVector->elementAt(4), destElements[4]);
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, destElements[i]);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3), true);
		ASSERT_EQUAL(toVector->length(), 5);

		ASSERT_EQUAL(toVector->elementAt(0), fromVector->elementAt(3));
		ASSERT_EQUAL(toVector->elementAt(1), fromVector->elementAt(4));
		ASSERT_EQUAL(toVector->elementAt(2), destElements[2]);
		ASSERT_EQUAL(toVector->elementAt(3), destElements[3]);
		ASSERT_EQUAL(toVector->elementAt(4), destElements[4]);
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, destElements[i]);
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		ASSERT_EQUAL(toVector->elementAt(0), fromVector->elementAt(3));
		ASSERT_EQUAL(toVector->elementAt(1), fromVector->elementAt(4));
		ASSERT_EQUAL(toVector->elementAt(2), destElements[2]);
		ASSERT_EQUAL(toVector->elementAt(3), destElements[3]);
		ASSERT_EQUAL(toVector->elementAt(4), destElements[4]);
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, destElements[i]);
		}

		ASSERT_EQUAL(toVector->replace(3, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		ASSERT_EQUAL(toVector->elementAt(0), destElements[0]);
		ASSERT_EQUAL(toVector->elementAt(1), destElements[1]);
		ASSERT_EQUAL(toVector->elementAt(2), destElements[2]);
		ASSERT_EQUAL(toVector->elementAt(3), fromVector->elementAt(3));
		ASSERT_EQUAL(toVector->elementAt(4), fromVector->elementAt(4));
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);

		ASSERT_EQUAL(toVector->replace(4, fromVector, 3, 5), false);
	}

	{
		VectorCell *toVector = VectorCell::fromFill(world, 5);

		ASSERT_EQUAL(toVector->replace(4, fromVector, 5, 3), false);
	}
}

void testFill(World &world)
{
	StringCell *originalElement = StringCell::fromUtf8StdString(world, "One");
	StringCell *fillElement = StringCell::fromUtf8StdString(world, "Two");

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement), true)

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), fillElement);
		}
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 0, 5), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), fillElement);
		}
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 3, 3), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), originalElement);
		}
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 5, 5), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), originalElement);
		}
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 4), true);

		ASSERT_EQUAL(testVector->elementAt(0), originalElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), fillElement);
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(true, testVector->fill(fillElement, 4));

		ASSERT_EQUAL(testVector->elementAt(0), originalElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), fillElement);
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 0, 1), true);

		ASSERT_EQUAL(testVector->elementAt(0), fillElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), originalElement);
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 5, 6), false);
	}

	{
		VectorCell *testVector = VectorCell::fromFill(world, 5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 3, 2), false);
	}
}

void testAll(World &world)
{
	testFromFill(world);
	testFromAppended(world);
	testSetElement(world);
	testCopy(world);
	testReplace(world);
	testFill(world);
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
