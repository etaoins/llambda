#include "binding/VectorCell.h"
#include "binding/StringCell.h"
#include "binding/UnitCell.h"

#include "core/init.h"
#include "assertions.h"

namespace
{
using namespace lliby;

void testFromFill()
{
	{
		VectorCell *emptyVector  = VectorCell::fromFill(0);

		ASSERT_EQUAL(emptyVector->length(), 0);
	}
	
	{
		VectorCell *unitVector  = VectorCell::fromFill(5);

		ASSERT_EQUAL(unitVector->length(), 5);
		ASSERT_EQUAL(unitVector->elementAt(0), UnitCell::instance());
		ASSERT_EQUAL(unitVector->elementAt(4), UnitCell::instance());
		ASSERT_EQUAL(unitVector->elementAt(5), 0);
	}
	
	{
		StringCell *testString = StringCell::fromUtf8CString(u8"Hello!");
		VectorCell *stringVector  = VectorCell::fromFill(4, testString);

		ASSERT_EQUAL(stringVector->length(), 4);
		ASSERT_EQUAL(stringVector->elementAt(0), testString);
		ASSERT_EQUAL(stringVector->elementAt(3), testString);
		ASSERT_EQUAL(stringVector->elementAt(4), 0);
	}
}

void testFromAppended()
{
	StringCell *string1 = StringCell::fromUtf8CString(u8"One");
	VectorCell *vector1 = VectorCell::fromFill(3, string1);

	StringCell *string2 = StringCell::fromUtf8CString(u8"Two");
	VectorCell *vector2 = VectorCell::fromFill(1, string2);

	StringCell *string3 = StringCell::fromUtf8CString(u8"Three");
	VectorCell *vector3 = VectorCell::fromFill(3, string3);

	{
		VectorCell *emptyVector = VectorCell::fromAppended({});

		ASSERT_EQUAL(emptyVector->length(), 0);
	}

	{
		VectorCell *appendedVector = VectorCell::fromAppended({vector1});

		ASSERT_EQUAL(appendedVector->length(), 3);
		ASSERT_EQUAL(appendedVector->elementAt(0), string1);
		ASSERT_EQUAL(appendedVector->elementAt(2), string1);
		ASSERT_EQUAL(appendedVector->elementAt(3), 0);
	}
	
	{
		VectorCell *appendedVector = VectorCell::fromAppended({vector1, vector2, vector3});

		ASSERT_EQUAL(appendedVector->length(), 7);
		ASSERT_EQUAL(appendedVector->elementAt(0), string1);
		ASSERT_EQUAL(appendedVector->elementAt(2), string1);
		ASSERT_EQUAL(appendedVector->elementAt(3), string2);
		ASSERT_EQUAL(appendedVector->elementAt(4), string3);
		ASSERT_EQUAL(appendedVector->elementAt(6), string3);
	}
}

void testSetElement()
{
	StringCell *testString = StringCell::fromUtf8CString(u8"Test");
	VectorCell *testVector  = VectorCell::fromFill(5);

	ASSERT_EQUAL(testVector->elementAt(0), UnitCell::instance());
	ASSERT_EQUAL(testVector->setElementAt(0, testString), true);
	ASSERT_EQUAL(testVector->elementAt(0), testString);
	
	ASSERT_EQUAL(testVector->elementAt(4), UnitCell::instance());
	ASSERT_EQUAL(testVector->setElementAt(4, testString), true);
	ASSERT_EQUAL(testVector->elementAt(4), testString);
	
	ASSERT_EQUAL(testVector->setElementAt(5, testString), false);
}

void testCopy()
{
	auto testVector = VectorCell::fromFill(5);

	for(unsigned int i = 0; i < 5; i++)
	{
		testVector->setElementAt(i, StringCell::fromUtf8CString("TEST"));
	}

	{
		VectorCell *wholeCopy = testVector->copy();

		ASSERT_EQUAL(wholeCopy->length(), 5);

		ASSERT_EQUAL(memcmp(wholeCopy->elements(), testVector->elements(), 5 * sizeof(DatumCell*)), 0);
	}
	
	{
		VectorCell *explicitWholeCopy = testVector->copy(0, 5);

		ASSERT_EQUAL(explicitWholeCopy->length(), 5);
		ASSERT_EQUAL(memcmp(explicitWholeCopy->elements(), testVector->elements(), 5 * sizeof(DatumCell*)), 0);
	}
	
	{
		VectorCell *firstTwoCopy = testVector->copy(0, 2);

		ASSERT_EQUAL(firstTwoCopy->length(), 2);
		ASSERT_EQUAL(firstTwoCopy->elementAt(0), testVector->elementAt(0));
		ASSERT_EQUAL(firstTwoCopy->elementAt(1), testVector->elementAt(1));
	}
	
	{
		VectorCell *lastTwoCopy = testVector->copy(3, 5);

		ASSERT_EQUAL(lastTwoCopy->length(), 2);
		ASSERT_EQUAL(lastTwoCopy->elementAt(0), testVector->elementAt(3));
		ASSERT_EQUAL(lastTwoCopy->elementAt(1), testVector->elementAt(4));
	}
	
	{
		VectorCell *emptyCopy = testVector->copy(3, 3);

		ASSERT_EQUAL(emptyCopy->length(), 0);
	}
}

void testReplace()
{
	auto *fromVector = VectorCell::fromFill(5); 
	for(unsigned int i = 0; i < 5; i++)
	{
		fromVector->setElementAt(i, StringCell::fromUtf8CString("TEST"));
	}
	
	DatumCell *destElements[5];
	for(unsigned int i = 0; i < 5; i++)
	{
		destElements[i] = StringCell::fromUtf8CString("TEST");
	}

	{
		VectorCell *toVector = VectorCell::fromFill(5); 
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, StringCell::fromUtf8CString("TEST"));
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector), true);
		ASSERT_EQUAL(toVector->length(), 5);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(toVector->elementAt(i), fromVector->elementAt(i));
		}
	}
	
	{
		VectorCell *toVector = VectorCell::fromFill(5); 
		for(unsigned int i = 0; i < 5; i++)
		{
			toVector->setElementAt(i, StringCell::fromUtf8CString("TEST"));
		}

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);
		
		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(toVector->elementAt(i), fromVector->elementAt(i));
		}
	}
	
	{
		VectorCell *toVector = VectorCell::fromFill(5); 
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
		VectorCell *toVector = VectorCell::fromFill(5); 
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
		VectorCell *toVector = VectorCell::fromFill(5); 
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
		VectorCell *toVector = VectorCell::fromFill(5); 
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
		VectorCell *toVector = VectorCell::fromFill(5); 
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
		VectorCell *toVector = VectorCell::fromFill(5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 3, 5), false);
	}
	
	{
		VectorCell *toVector = VectorCell::fromFill(5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 5, 3), false);
	}
}

void testFill()
{
	StringCell *originalElement = StringCell::fromUtf8CString("One");
	StringCell *fillElement = StringCell::fromUtf8CString("Two");

	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement), true)

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), fillElement);
		}
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 0, 5), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), fillElement);
		}
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 3, 3), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), originalElement);
		}
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 5, 5), true);

		for(unsigned int i = 0; i < 5; i++)
		{
			ASSERT_EQUAL(testVector->elementAt(i), originalElement);
		}
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 4), true);

		ASSERT_EQUAL(testVector->elementAt(0), originalElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), fillElement);
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(true, testVector->fill(fillElement, 4));

		ASSERT_EQUAL(testVector->elementAt(0), originalElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), fillElement);
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 0, 1), true);

		ASSERT_EQUAL(testVector->elementAt(0), fillElement);
		ASSERT_EQUAL(testVector->elementAt(1), originalElement);
		ASSERT_EQUAL(testVector->elementAt(2), originalElement);
		ASSERT_EQUAL(testVector->elementAt(3), originalElement);
		ASSERT_EQUAL(testVector->elementAt(4), originalElement);
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 5, 6), false);
	}
	
	{
		VectorCell *testVector = VectorCell::fromFill(5, originalElement);

		ASSERT_EQUAL(testVector->fill(fillElement, 3, 2), false);
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testFromFill();
	testFromAppended();
	testSetElement();
	testCopy();
	testReplace();
	testFill();

	return 0;
}
