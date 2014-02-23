#include <string.h>

#include "binding/BytevectorCell.h"
#include "binding/StringCell.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "alloc/StrongRef.h"

namespace
{
using namespace lliby;

void testFromFill(World &world)
{
	{
		BytevectorCell *emptyVector  = BytevectorCell::fromFill(world, 0);

		ASSERT_EQUAL(emptyVector->length(), 0);
	}
	
	{
		BytevectorCell *zeroFillVector  = BytevectorCell::fromFill(world, 8);
		const uint8_t expectedData[8] = { 0 };

		ASSERT_EQUAL(zeroFillVector->length(), 8);
		ASSERT_EQUAL(memcmp(zeroFillVector->data(), expectedData, 8), 0);
	}
	
	{
		BytevectorCell *sevenFillVector  = BytevectorCell::fromFill(world, 4, 7);
		const uint8_t expectedData[4] = { 7, 7, 7, 7 };

		ASSERT_EQUAL(sevenFillVector->length(), 4);
		ASSERT_EQUAL(memcmp(sevenFillVector->data(), expectedData, 4), 0);
	}
}

void testFromAppended(World &world)
{
	uint8_t vector1Data[3] = { 100, 101, 102 };
	alloc::StrongRef<BytevectorCell> vector1(world, BytevectorCell::fromUnownedData(world, vector1Data, sizeof(vector1Data)));

	uint8_t vector2Data[1] = { 0 };
	alloc::StrongRef<BytevectorCell> vector2(world, BytevectorCell::fromUnownedData(world, vector2Data, sizeof(vector2Data)));

	uint8_t vector3Data[3] = { 200, 201, 202 };
	alloc::StrongRef<BytevectorCell> vector3(world, BytevectorCell::fromUnownedData(world, vector3Data, sizeof(vector3Data)));

	{
		BytevectorCell *emptyVector = BytevectorCell::fromAppended(world, {});

		ASSERT_EQUAL(emptyVector->length(), 0);
	}
	
	{
		BytevectorCell *appendedVector = BytevectorCell::fromAppended(world, {vector1});

		ASSERT_EQUAL(appendedVector->length(), 3);
		
		const uint8_t expectedData[3] = {100, 101, 102};
		ASSERT_EQUAL(memcmp(appendedVector->data(), expectedData, 3), 0);
	}
	
	{
		BytevectorCell *appendedVector = BytevectorCell::fromAppended(world, {vector1, vector2, vector3});

		ASSERT_EQUAL(appendedVector->length(), 7);
		
		const uint8_t expectedData[7] = {100, 101, 102, 0, 200, 201, 202};
		ASSERT_EQUAL(memcmp(appendedVector->data(), expectedData, 7), 0);
	}
}

void testByteAccess(World &world)
{
	uint8_t vectorData[5] = { 0, 1, 2, 3, 4 };

	auto *testVector = BytevectorCell::fromUnownedData(world, vectorData, sizeof(vectorData));

	ASSERT_EQUAL(testVector->byteAt(0), 0);
	ASSERT_EQUAL(testVector->byteAt(4), 4);
	ASSERT_EQUAL(testVector->byteAt(5), BytevectorCell::InvalidByte);

	ASSERT_EQUAL(testVector->setByteAt(0, 128), true);
	ASSERT_EQUAL(testVector->byteAt(0), 128);

	ASSERT_EQUAL(testVector->setByteAt(4, 255), true);
	ASSERT_EQUAL(testVector->byteAt(4), 255);
	
	ASSERT_EQUAL(testVector->setByteAt(5, 255), false);
}

void testCopy(World &world)
{
	uint8_t vectorData[5] = { 0, 1, 2, 3, 4 };

	alloc::StrongRef<BytevectorCell> testVector(world, BytevectorCell::fromUnownedData(world, vectorData, sizeof(vectorData)));

	{
		BytevectorCell *wholeCopy = testVector->copy(world);

		ASSERT_EQUAL(wholeCopy->length(), 5);

		ASSERT_EQUAL(memcmp(wholeCopy->data(), vectorData, 5), 0);
	}
	
	{
		BytevectorCell *explicitWholeCopy = testVector->copy(world, 0, 5);

		ASSERT_EQUAL(explicitWholeCopy->length(), 5);
		ASSERT_EQUAL(memcmp(explicitWholeCopy->data(), vectorData, 5), 0);
	}
	
	{
		BytevectorCell *firstTwoCopy = testVector->copy(world, 0, 2);

		ASSERT_EQUAL(firstTwoCopy->length(), 2);
		ASSERT_EQUAL(firstTwoCopy->byteAt(0), 0);
		ASSERT_EQUAL(firstTwoCopy->byteAt(1), 1);
	}
	
	{
		BytevectorCell *lastTwoCopy = testVector->copy(world, 3, 5);

		ASSERT_EQUAL(lastTwoCopy->length(), 2);
		ASSERT_EQUAL(lastTwoCopy->byteAt(0), 3);
		ASSERT_EQUAL(lastTwoCopy->byteAt(1), 4);
	}
	
	{
		BytevectorCell *emptyCopy = testVector->copy(world, 3, 3);

		ASSERT_EQUAL(emptyCopy->length(), 0);
	}
}

void testReplace(World &world)
{
	uint8_t fromData[5] = { 200, 201, 202, 203, 204 };
	alloc::StrongRef<BytevectorCell> fromVector(world, BytevectorCell::fromUnownedData(world, fromData, 5)); 

	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector), true);
		ASSERT_EQUAL(toVector->length(), 5);
		ASSERT_EQUAL(memcmp(toVector->data(), fromVector->data(), 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);
		ASSERT_EQUAL(memcmp(toVector->data(), fromVector->data(), 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 2, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {100, 101, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 0, 2), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {200, 201, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {203, 204, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(0, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {203, 204, 102, 103, 104 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(3, fromVector, 3, 5), true);
		ASSERT_EQUAL(toVector->length(), 5);

		const uint8_t expectedData[5] = {100, 101, 102, 203, 204 };
		ASSERT_EQUAL(memcmp(toVector->data(), expectedData, 5), 0);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 3, 5), false);
	}
	
	{
		uint8_t toData[5] = { 100, 101, 102, 103, 104 };
		auto *toVector = BytevectorCell::fromUnownedData(world, toData, 5); 

		ASSERT_EQUAL(toVector->replace(4, fromVector, 5, 3), false);
	}
}

void testUtf8ToString(World &world)
{
	auto stringData = reinterpret_cast<std::uint8_t*>(strdup(u8"Hello ☃!"));
	alloc::StrongRef<BytevectorCell> sourceVector(world, BytevectorCell::fromUnownedData(world, stringData, 10));

	{
		StringCell *fullString = sourceVector->utf8ToString(world);

		ASSERT_EQUAL(fullString->byteLength(), 10);
		ASSERT_EQUAL(fullString->charLength(), 8);
		ASSERT_EQUAL(memcmp(fullString->utf8Data(), u8"Hello ☃!", 11), 0);

	}
	
	{
		StringCell *fullString = sourceVector->utf8ToString(world, 0, 10);

		ASSERT_EQUAL(fullString->byteLength(), 10);
		ASSERT_EQUAL(fullString->charLength(), 8);
		ASSERT_EQUAL(memcmp(fullString->utf8Data(), u8"Hello ☃!", 11), 0);
	}
	
	{
		StringCell *emptyString = sourceVector->utf8ToString(world, 7, 7);

		ASSERT_EQUAL(emptyString->byteLength(), 0);
		ASSERT_EQUAL(emptyString->charLength(), 0);
		ASSERT_EQUAL(memcmp(emptyString->utf8Data(), u8"", 1), 0);
	}
	
	{
		StringCell *helloString = sourceVector->utf8ToString(world, 0, 5);

		ASSERT_EQUAL(helloString->byteLength(), 5);
		ASSERT_EQUAL(helloString->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloString->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		StringCell *endString = sourceVector->utf8ToString(world, 6, 10);

		ASSERT_EQUAL(endString->byteLength(), 4);
		ASSERT_EQUAL(endString->charLength(), 2);
		ASSERT_EQUAL(memcmp(endString->utf8Data(), u8"☃!", 5), 0);
	}
	
	{
		StringCell *invalidString = sourceVector->utf8ToString(world, 7, 12);
		ASSERT_EQUAL(invalidString, 0);
	}
	
	{
		StringCell *invalidString = sourceVector->utf8ToString(world, 6, 4);
		ASSERT_EQUAL(invalidString, 0);
	}
	
	{
		StringCell *invalidString = sourceVector->utf8ToString(world, 11);
		ASSERT_EQUAL(invalidString, 0);
	}

	free(stringData);
}

void testAll(World &world)
{
	testFromFill(world);
	testFromAppended(world);
	testByteAccess(world);
	testCopy(world);
	testReplace(world);
	testUtf8ToString(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);

	return 0;
}
