#include <string.h>

#include "binding/BoxedByteVector.h"
#include "binding/BoxedString.h"

#include "core/init.h"
#include "assertions.h"

namespace
{
using namespace lliby;

void testFromFill()
{
	{
		BoxedByteVector *emptyVector  = BoxedByteVector::fromFill(0);

		ASSERT_EQUAL(emptyVector->length(), 0);
	}
	
	{
		BoxedByteVector *zeroFillVector  = BoxedByteVector::fromFill(8);
		const uint8_t expectedData[8] = { 0 };

		ASSERT_EQUAL(zeroFillVector->length(), 8);
		ASSERT_EQUAL(memcmp(zeroFillVector->data(), expectedData, 8), 0);
	}
	
	{
		BoxedByteVector *sevenFillVector  = BoxedByteVector::fromFill(4, 7);
		const uint8_t expectedData[4] = { 7, 7, 7, 7 };

		ASSERT_EQUAL(sevenFillVector->length(), 4);
		ASSERT_EQUAL(memcmp(sevenFillVector->data(), expectedData, 4), 0);
	}
}

void testFromAppended()
{
	uint8_t vector1Data[3] = { 100, 101, 102 };
	auto vector1 = new BoxedByteVector(vector1Data, sizeof(vector1Data));

	uint8_t vector2Data[1] = { 0 };
	auto vector2 = new BoxedByteVector(vector2Data, sizeof(vector2Data));

	uint8_t vector3Data[3] = { 200, 201, 202 };
	auto vector3 = new BoxedByteVector(vector3Data, sizeof(vector3Data));

	{
		BoxedByteVector *emptyVector = BoxedByteVector::fromAppended({});

		ASSERT_EQUAL(emptyVector->length(), 0);
	}
	
	{
		BoxedByteVector *appendedVector = BoxedByteVector::fromAppended({vector1});

		ASSERT_EQUAL(appendedVector->length(), 3);
		
		const uint8_t expectedData[3] = {100, 101, 102};
		ASSERT_EQUAL(memcmp(appendedVector->data(), expectedData, 3), 0);
	}
	
	{
		BoxedByteVector *appendedVector = BoxedByteVector::fromAppended({vector1, vector2, vector3});

		ASSERT_EQUAL(appendedVector->length(), 7);
		
		const uint8_t expectedData[7] = {100, 101, 102, 0, 200, 201, 202};
		ASSERT_EQUAL(memcmp(appendedVector->data(), expectedData, 7), 0);
	}
}

void testByteAccess()
{
	uint8_t vectorData[5] = { 0, 1, 2, 3, 4 };

	auto *testVector = new BoxedByteVector(vectorData, sizeof(vectorData));

	ASSERT_EQUAL(testVector->byteAt(0), 0);
	ASSERT_EQUAL(testVector->byteAt(4), 4);
	ASSERT_EQUAL(testVector->byteAt(5), BoxedByteVector::InvalidByte);

	ASSERT_EQUAL(testVector->setByteAt(0, 128), true);
	ASSERT_EQUAL(testVector->byteAt(0), 128);

	ASSERT_EQUAL(testVector->setByteAt(4, 255), true);
	ASSERT_EQUAL(testVector->byteAt(4), 255);
	
	ASSERT_EQUAL(testVector->setByteAt(5, 255), false);
}

void testCopy()
{
	uint8_t vectorData[5] = { 0, 1, 2, 3, 4 };

	auto *testVector = new BoxedByteVector(vectorData, sizeof(vectorData));

	{
		BoxedByteVector *wholeCopy = testVector->copy();

		ASSERT_EQUAL(wholeCopy->length(), 5);

		ASSERT_EQUAL(memcmp(wholeCopy->data(), vectorData, 5), 0);
	}
	
	{
		BoxedByteVector *explicitWholeCopy = testVector->copy(0, 5);

		ASSERT_EQUAL(explicitWholeCopy->length(), 5);
		ASSERT_EQUAL(memcmp(explicitWholeCopy->data(), vectorData, 5), 0);
	}
	
	{
		BoxedByteVector *firstTwoCopy = testVector->copy(0, 2);

		ASSERT_EQUAL(firstTwoCopy->length(), 2);
		ASSERT_EQUAL(firstTwoCopy->byteAt(0), 0);
		ASSERT_EQUAL(firstTwoCopy->byteAt(1), 1);
	}
	
	{
		BoxedByteVector *lastTwoCopy = testVector->copy(3, 5);

		ASSERT_EQUAL(lastTwoCopy->length(), 2);
		ASSERT_EQUAL(lastTwoCopy->byteAt(0), 3);
		ASSERT_EQUAL(lastTwoCopy->byteAt(1), 4);
	}
	
	{
		BoxedByteVector *emptyCopy = testVector->copy(3, 3);

		ASSERT_EQUAL(emptyCopy->length(), 0);
	}
}

void testReplace()
{
	uint8_t fromData[5] = { 200, 201, 202, 203, 204 };
	const auto *fromVector = new BoxedByteVector(fromData, 5); 

	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector), true);
		ASSERT_EQUAL(toVector->length(), 5);
		ASSERT_EQUAL(memcmp(toVector->data(), fromVector->data(), 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);
		ASSERT_EQUAL(memcmp(toVector->data(), fromVector->data(), 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 2, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {100, 101, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {200, 201, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {203, 204, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {203, 204, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(3, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {100, 101, 102, 203, 204 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 3, 5), false);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = new BoxedByteVector(toData, 5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 5, 3), false);
	}
}

void testUtf8ToString()
{
	auto stringData = reinterpret_cast<std::uint8_t*>(strdup(u8"Hello ☃!"));
	auto sourceVector = new BoxedByteVector(stringData, 10);

	{
		BoxedString *fullString = sourceVector->utf8ToString();

		ASSERT_EQUAL(fullString->byteLength(), 10);
		ASSERT_EQUAL(fullString->charLength(), 8);
		ASSERT_EQUAL(memcmp(fullString->utf8Data(), u8"Hello ☃!", 11), 0);

	}
	
	{
		BoxedString *fullString = sourceVector->utf8ToString(0, 10);

		ASSERT_EQUAL(fullString->byteLength(), 10);
		ASSERT_EQUAL(fullString->charLength(), 8);
		ASSERT_EQUAL(memcmp(fullString->utf8Data(), u8"Hello ☃!", 11), 0);
	}
	
	{
		BoxedString *emptyString = sourceVector->utf8ToString(7, 7);

		ASSERT_EQUAL(emptyString->byteLength(), 0);
		ASSERT_EQUAL(emptyString->charLength(), 0);
		ASSERT_EQUAL(memcmp(emptyString->utf8Data(), u8"", 1), 0);
	}
	
	{
		BoxedString *helloString = sourceVector->utf8ToString(0, 5);

		ASSERT_EQUAL(helloString->byteLength(), 5);
		ASSERT_EQUAL(helloString->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloString->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		BoxedString *endString = sourceVector->utf8ToString(6, 10);

		ASSERT_EQUAL(endString->byteLength(), 4);
		ASSERT_EQUAL(endString->charLength(), 2);
		ASSERT_EQUAL(memcmp(endString->utf8Data(), u8"☃!", 5), 0);
	}
	
	{
		BoxedString *invalidString = sourceVector->utf8ToString(7, 12);
		ASSERT_EQUAL(invalidString, 0);
	}
	
	{
		BoxedString *invalidString = sourceVector->utf8ToString(6, 4);
		ASSERT_EQUAL(invalidString, 0);
	}
	
	{
		BoxedString *invalidString = sourceVector->utf8ToString(11);
		ASSERT_EQUAL(invalidString, 0);
	}

	free(stringData);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testFromFill();
	testFromAppended();
	testByteAccess();
	testCopy();
	testReplace();
	testUtf8ToString();

	return 0;
}
