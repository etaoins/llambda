#include <string.h>

#include "binding/StringCell.h"
#include "binding/BytevectorCell.h"
#include "util/StringCellBuilder.h"

#include "core/init.h"
#include "core/World.h"
#include "assertions.h"
#include "stubdefinitions.h"
#include "platform/memory.h"

#include "unicode/utf8/InvalidByteSequenceException.h"

#include "alloc/cellref.h"

namespace
{
using namespace lliby;

std::ostream& operator<<(std::ostream &stream, const lliby::UnicodeChar &unicodeChar)
{
	stream << std::hex << unicodeChar.codePoint();
	return stream;
}

std::uint8_t* utf8Bytes(const char *str)
{
	return (std::uint8_t*)(str);
}

// Finds an string size in bytes that will result in no allocation slack
// This is useful for checking for boundary conditions
std::size_t slacklessStringSize(size_t minimumSize)
{
	platform::SizedMallocResult testAllocResult = platform::sizedMalloc(minimumSize);
	const size_t testSize = testAllocResult.actualSize;
	free(testAllocResult.basePointer);

	return testSize;
}

void testFromUtf8CString(World &world)
{
	{
		StringCell *emptyValue = StringCell::fromUtf8CString(world, u8"");

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->constUtf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->charLength(), 5);
	}
	
	{
		StringCell *highUnicodeValue = StringCell::fromUtf8CString(world, u8"☃🐉");

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}

	{
		bool caughtException = false;

		try
		{
			// Truncated sequence
			StringCell::fromUtf8CString(world, "H\xE2\x98");
		}
		catch(const utf8::InvalidByteSequenceException &e)
		{
			ASSERT_EQUAL(e.byteOffset(), 2);
			caughtException = true;
		}

		ASSERT_TRUE(caughtException);
	}
}

void testFromUtf8Data(World &world)
{
	{
		StringCell *emptyValue = StringCell::fromUtf8Data(world, nullptr, 0);

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		// Intentionally include NULL as a char to make sure we're NULL safe
		auto helloBytes = reinterpret_cast<const std::uint8_t*>(u8"Hello");
		StringCell *helloValue = StringCell::fromUtf8Data(world, helloBytes, 6);

		ASSERT_EQUAL(helloValue->byteLength(), 6);
		ASSERT_EQUAL(helloValue->constUtf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->constUtf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->charLength(), 6);;
	}
	
	{
		auto highUnicodeBytes = reinterpret_cast<const std::uint8_t*>(u8"☃🐉");
		StringCell *highUnicodeValue = StringCell::fromUtf8Data(world, highUnicodeBytes, 7);

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}

	{
		bool caughtException = false;

		try
		{
			// This ends with a truncated 3 byte sequence
			auto truncatedThreeBytes = reinterpret_cast<const std::uint8_t*>("H\xE2\x98");
			StringCell::fromUtf8Data(world, truncatedThreeBytes, 3);
		}
		catch(utf8::InvalidByteSequenceException &e)
		{
			ASSERT_EQUAL(e.byteOffset(), 2);
			caughtException = true;
		}

		ASSERT_TRUE(caughtException);
	}

	{
		const size_t testSize = slacklessStringSize(24);

		auto *testCharData = new unsigned char[testSize];
		memset(testCharData, 'a', testSize);
		StringCell *offByOneTest = StringCell::fromUtf8Data(world, testCharData, testSize);

		ASSERT_EQUAL(offByOneTest->byteLength(), testSize);
		ASSERT_EQUAL(offByOneTest->charLength(), testSize);
		ASSERT_EQUAL(memcmp(offByOneTest->constUtf8Data(), testCharData, testSize), 0);

		delete[] testCharData;
	}
}

void testFromUtf8StdString(World &world)
{
	// Include a NULL character to ensure the conversion is NULL safe
	const char *nullStringData = "Hell\0o";
	std::string nullStdString(nullStringData, 6);
	StringCell *nullValue = StringCell::fromUtf8StdString(world, nullStdString);

	ASSERT_EQUAL(nullValue->byteLength(), 6);
	ASSERT_EQUAL(nullValue->charLength(), 6);
	ASSERT_EQUAL(memcmp(nullValue->constUtf8Data(), nullStringData, 6), 0);
}

void testCompare(World &world)
{
	alloc::StringRef hello1(world, StringCell::fromUtf8CString(world, "Hello"));
	alloc::StringRef hello2(world, StringCell::fromUtf8Data(world, utf8Bytes("Hello"), 5));
	alloc::StringRef HELLO(world, StringCell::fromUtf8CString(world, "HELLO"));
	alloc::StringRef worldString(world, StringCell::fromUtf8CString(world, "worldString"));
	alloc::StringRef nulledHello1(world, StringCell::fromUtf8Data(world, utf8Bytes("Hell\0o"), 6));
	alloc::StringRef nulledHello2(world, StringCell::fromUtf8Data(world, utf8Bytes("Hell\0o"), 6));
	alloc::StringRef hell(world, StringCell::fromUtf8CString(world, "Hell"));
	alloc::StringRef unicodeValue(world, StringCell::fromUtf8CString(world, u8"☃🐉"));
	alloc::StringRef lowercaseUnicode(world, StringCell::fromUtf8CString(world, u8"сфmmциist gязэtiйgs!"));
	alloc::StringRef uppercaseUnicode(world, StringCell::fromUtf8CString(world, u8"СФMMЦИIST GЯЗЭTIЙGS!"));

	ASSERT_TRUE(*hello1 == *hello1); 
	ASSERT_TRUE(hello1->compare(hello1) == 0);
	
	// Ensure != works
	ASSERT_FALSE(*hello1 != *hello1); 

	// Ensure different instances with the same content are equal
	ASSERT_TRUE(*hello1 == *hello2);
	ASSERT_TRUE(hello1->compare(hello2) == 0);

	// Ensure inequal strings are considered equal
	ASSERT_FALSE(*hello1 == *worldString);
	ASSERT_TRUE(hello1->compare(worldString) < 0);

	// Make sure strings with nulls in them are compared correctly
	ASSERT_TRUE(*nulledHello1 == *nulledHello2);
	ASSERT_TRUE(nulledHello1->compare(nulledHello2) == 0);

	// Ensure the comparison doesn't stop on the first NULL
	// Also ensure that shorter strings sort before longer strings that have 
	// them as a prefix
	ASSERT_FALSE(*nulledHello1 == *hell);
	ASSERT_TRUE(nulledHello1->compare(hell) > 0);

	ASSERT_TRUE(hell->compare(nulledHello1) < 0);
	ASSERT_TRUE(hell->compare(nulledHello1, CaseSensitivity::Insensitive) < 0);

	// Make sure high Unicode code points sort after ASCII
	ASSERT_TRUE(unicodeValue->compare(hello1) > 0);
	ASSERT_TRUE(unicodeValue->compare(hello1, CaseSensitivity::Insensitive) > 0);

	// Make sure case sensitivity works on ASCII strings
	ASSERT_TRUE(hello1->compare(HELLO) > 0);
	ASSERT_TRUE(hello1->compare(HELLO, CaseSensitivity::Insensitive) == 0);

	// Make sure case senstivity works with Unicode strings
	ASSERT_TRUE(lowercaseUnicode->compare(uppercaseUnicode) > 0);
	ASSERT_TRUE(lowercaseUnicode->compare(uppercaseUnicode, CaseSensitivity::Insensitive) == 0);

	// Make sure case sensitivity doesn't affect Unicode symbols
	ASSERT_TRUE(unicodeValue->compare(unicodeValue) == 0);
	ASSERT_TRUE(unicodeValue->compare(unicodeValue, CaseSensitivity::Insensitive) == 0);
}

void testCharAt(World &world)
{
	{
		StringCell *emptyValue = StringCell::fromUtf8CString(world, u8"");

		ASSERT_FALSE(emptyValue->charAt(0).isValid());
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->charAt(0), UnicodeChar('H'));
		ASSERT_EQUAL(helloValue->charAt(4), UnicodeChar('o'));
		ASSERT_FALSE(helloValue->charAt(5).isValid());
		ASSERT_FALSE(helloValue->charAt(1024).isValid());
	}

	{
		StringCell *highUnicodeValue = StringCell::fromUtf8CString(world, u8"☃🐉");

		ASSERT_EQUAL(highUnicodeValue->charAt(0), UnicodeChar(0x02603));
		ASSERT_EQUAL(highUnicodeValue->charAt(1), UnicodeChar(0x1F409));
		ASSERT_FALSE(highUnicodeValue->charAt(2).isValid());
		ASSERT_FALSE(highUnicodeValue->charAt(1024).isValid());
	}
}

void testFromFill(World &world)
{
	{
		StringCell *emptyAsciiValue = StringCell::fromFill(world, 0, UnicodeChar(0));

		ASSERT_EQUAL(emptyAsciiValue->byteLength(), 0);
		ASSERT_EQUAL(emptyAsciiValue->charLength(), 0);
	}
	
	{
		StringCell *emptyUnicodeValue = StringCell::fromFill(world, 0, UnicodeChar(0x02603));

		ASSERT_EQUAL(emptyUnicodeValue->byteLength(), 0);
		ASSERT_EQUAL(emptyUnicodeValue->charLength(), 0);
	}
	
	{
		StringCell *asciiValue = StringCell::fromFill(world, 5, UnicodeChar('H'));

		ASSERT_EQUAL(asciiValue->byteLength(), 5);
		ASSERT_EQUAL(asciiValue->charLength(), 5);
		ASSERT_EQUAL(asciiValue->charAt(0), UnicodeChar('H'));
		ASSERT_EQUAL(asciiValue->charAt(4), UnicodeChar('H'));
		ASSERT_FALSE(asciiValue->charAt(5).isValid());
	}
	
	{
		StringCell *unicodeValue = StringCell::fromFill(world, 5, UnicodeChar(0x02603));

		ASSERT_EQUAL(unicodeValue->byteLength(), 15);
		ASSERT_EQUAL(unicodeValue->charLength(), 5);
		ASSERT_EQUAL(unicodeValue->charAt(0), UnicodeChar(0x02603));
		ASSERT_EQUAL(unicodeValue->charAt(4), UnicodeChar(0x02603));
		ASSERT_FALSE(unicodeValue->charAt(5).isValid());
	}
}

void testFromAppended(World &world)
{
	{
		StringCell *emptyValue = StringCell::fromAppended(world, std::vector<StringCell*>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}
	
	{
		alloc::StringRef part1(world, StringCell::fromUtf8CString(world, u8"Hello"));
		alloc::StringRef part2(world, StringCell::fromUtf8CString(world, u8" "));
		alloc::StringRef part3(world, StringCell::fromUtf8CString(world, u8"world!"));

		std::vector<StringCell*> appendParts = {part1, part2, part3};

		StringCell *asciiValue = StringCell::fromAppended(world, appendParts);
		
		ASSERT_EQUAL(asciiValue->byteLength(), 12);
		ASSERT_EQUAL(asciiValue->charLength(), 12);
		ASSERT_EQUAL(memcmp(asciiValue->constUtf8Data(), u8"Hello world!", 12), 0);
	}
	
	{
		alloc::StringRef part1(world, StringCell::fromUtf8CString(world, u8"Hello "));
		alloc::StringRef part2(world, StringCell::fromUtf8CString(world, u8"☃"));

		std::vector<StringCell*> appendParts = {part1, part2};

		StringCell *unicodeValue = StringCell::fromAppended(world, appendParts);
		
		ASSERT_EQUAL(unicodeValue->byteLength(), 9);
		ASSERT_EQUAL(unicodeValue->charLength(), 7);
		ASSERT_EQUAL(memcmp(unicodeValue->constUtf8Data(), "Hello ☃", 9), 0);
	}
}

void testStringCellBuilder(World &world)
{
	{
		StringCellBuilder builder(0);
		StringCell *emptyValue = builder.result(world);

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		StringCellBuilder builder(5);

		builder << UnicodeChar('H');
		builder << UnicodeChar('e');
		builder << UnicodeChar('l');
		builder << UnicodeChar('l');
		builder << UnicodeChar('o');

		StringCell *helloValue = builder.result(world);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"Hello", 5), 0);
	}

	{
		StringCellBuilder builder(3);

		builder << UnicodeChar(0x1F409);
		builder << UnicodeChar(0x02603);
		builder << UnicodeChar('!');

		StringCell *unicodeValue = builder.result(world);

		ASSERT_EQUAL(unicodeValue->byteLength(), 8);
		ASSERT_EQUAL(unicodeValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(unicodeValue->constUtf8Data(), u8"🐉☃!", 8), 0);
	}

	{
		const size_t testSize = slacklessStringSize(24);

		StringCellBuilder builder(testSize);

		for(auto i = 0; i < testSize; i++)
		{
			builder << UnicodeChar(0x20);
		}

		StringCell *offByOneTest = builder.result(world);

		ASSERT_EQUAL(offByOneTest->byteLength(), testSize);
		ASSERT_EQUAL(offByOneTest->charLength(), testSize);
	}
}

void testStringCopy(World &world)
{
	{
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *helloCopy = helloValue->copy(world);

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->constUtf8Data(), u8"Hello", 5), 0);
	}
	
	{
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *elloCopy = helloValue->copy(world, 1);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->constUtf8Data(), u8"ello", 4), 0);
	}
	
	{
		// Make sure there's no boundry condition on the last character
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *elloCopy = helloValue->copy(world, 1, 5);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->constUtf8Data(), u8"ello", 4), 0);
	}
	
	{
		// Allow empty strings
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *emptyCopy = helloValue->copy(world, 0, 0);

		ASSERT_EQUAL(emptyCopy->byteLength(), 0);
		ASSERT_EQUAL(emptyCopy->charLength(), 0);
	}
	
	{
		// Allow empty from the very end
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *emptyCopy = helloValue->copy(world, 5, 5);

		ASSERT_EQUAL(emptyCopy->byteLength(), 0);
		ASSERT_EQUAL(emptyCopy->charLength(), 0);
	}
	
	{
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *ellCopy = helloValue->copy(world, 1, 4);

		ASSERT_EQUAL(ellCopy->byteLength(), 3);
		ASSERT_EQUAL(ellCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(ellCopy->constUtf8Data(), u8"ell", 3), 0);
	}
	
	{
		// Off the end
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *invalidCopy = helloValue->copy(world, 0, 16);

		ASSERT_NULL(invalidCopy);
	}
	
	{
		// start > end
		alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello"));
		StringCell *invalidCopy = helloValue->copy(world, 3, 2);

		ASSERT_NULL(invalidCopy);
	}

	{
		alloc::StringRef japanValue(world, StringCell::fromUtf8CString(world, u8"日本国"));
		StringCell *japanCopy = japanValue->copy(world);

		ASSERT_EQUAL(japanCopy->byteLength(), 9);
		ASSERT_EQUAL(japanCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanCopy->constUtf8Data(), u8"日本国", 9), 0);
	}
	
	{
		alloc::StringRef japanValue(world, StringCell::fromUtf8CString(world, u8"日本国"));
		StringCell *japanCopy = japanValue->copy(world, 1);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->constUtf8Data(), u8"本国", 6), 0);
	}
	
	{
		alloc::StringRef japanValue(world, StringCell::fromUtf8CString(world, u8"日本国"));
		// Check for the same boundry in Unicode
		StringCell *japanCopy = japanValue->copy(world, 1, 3);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->constUtf8Data(), u8"本国", 6), 0);
	}
	
	{
		alloc::StringRef japanValue(world, StringCell::fromUtf8CString(world, u8"日本国"));
		StringCell *japanCopy = japanValue->copy(world, 1, 2);

		ASSERT_EQUAL(japanCopy->byteLength(), 3);
		ASSERT_EQUAL(japanCopy->charLength(), 1);
		ASSERT_EQUAL(memcmp(japanCopy->constUtf8Data(), u8"本", 3), 0);
	}
	
	{
		alloc::StringRef mixedValue(world, StringCell::fromUtf8CString(world, u8"日Hello国"));
		StringCell *helloCopy = mixedValue->copy(world, 1, 6);

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->constUtf8Data(), u8"Hello", 5), 0);
	}

	{
		const size_t testSize = slacklessStringSize(24);

		// Allocate an extra by so copy() needs to COW
		StringCell *offByOneTest = StringCell::fromFill(world, testSize + 1, UnicodeChar(0x20));
		StringCell *offByOneCopy = offByOneTest->copy(world, 0, testSize);

		ASSERT_EQUAL(offByOneCopy->byteLength(), testSize);
		ASSERT_EQUAL(offByOneCopy->charLength(), testSize);
	}
}

void testSetCharAt(World &world)
{
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(0, UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"Yello", 5), 0);
		
		// Going off the end of the string should fail
		ASSERT_EQUAL(helloValue->setCharAt(5, UnicodeChar('Y')), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(1, UnicodeChar(0)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(helloValue->charAt(1), UnicodeChar(0));
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(3, UnicodeChar(0x1F409)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 8);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"Hel🐉o", 8), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(5, UnicodeChar('Y')), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"日本国");

		ASSERT_EQUAL(helloValue->setCharAt(1, UnicodeChar('O')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"日O国", 7), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(4, UnicodeChar('Y')), false);
	}
}

void testFill(World &world)
{
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"YYYYY", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"YYYYY", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 6), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 15);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"☃☃☃☃☃", 15), 0);
	}
	
	{
		// This also converts an inline string to a heap string
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603), 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 13);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"H☃☃☃☃", 13), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603), 1, 4), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"H☃☃☃o", 11), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 1, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"Hello", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"YYYYY", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"YYYYY", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"YYYY☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 4, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"YYYYY", 5), 0);
	}
	
	{
		// This also converts a heap string to an inline string
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->allocSlackBytes(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"☃YYYY", 7), 0);
	}
}

void testReplace(World &world)
{
	const alloc::StringRef constWorld(world, StringCell::fromUtf8CString(world, u8"world"));
	const alloc::StringRef constJapan(world, StringCell::fromUtf8CString(world, u8"日本国")); 

	{
		// From R7RS
		alloc::StringRef numbers(world, StringCell::fromUtf8CString(world, u8"12345"));
		alloc::StringRef letters(world, StringCell::fromUtf8CString(world, u8"abcde"));

		ASSERT_EQUAL(letters->replace(1, numbers, 0, 2), true);

		ASSERT_EQUAL(letters->byteLength(), 5);
		ASSERT_EQUAL(letters->charLength(), 5);
		ASSERT_EQUAL(memcmp(letters->constUtf8Data(), u8"a12de", 5), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constWorld), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"world", 5), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"日本国lo", 11), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(world, u8"日本国");

		ASSERT_EQUAL(japanValue->replace(0, constWorld, 0, 3), true);

		ASSERT_EQUAL(japanValue->byteLength(), 3);
		ASSERT_EQUAL(japanValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanValue->constUtf8Data(), u8"wor", 3), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(world, u8"日本国");

		// Overruns the string
		ASSERT_EQUAL(japanValue->replace(0, constWorld), false)
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"He日本国", 11), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		// Off the end
		ASSERT_EQUAL(helloValue->replace(3, constJapan), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 9);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->constUtf8Data(), u8"He本国o", 9), 0);
	}
	
	{
		StringCell *complexValue = StringCell::fromUtf8CString(world, u8"Hello 日本国");

		// We should be able to replace a substring from ourselves
		ASSERT_EQUAL(complexValue->replace(0, complexValue, 6), true);

		ASSERT_EQUAL(complexValue->byteLength(), 21);
		ASSERT_EQUAL(complexValue->charLength(), 9);
		ASSERT_EQUAL(memcmp(complexValue->constUtf8Data(), u8"日本国lo 日本国", 21), 0);
	}
}

void testUnicodeChars(World &world)
{
	StringCell *helloValue = StringCell::fromUtf8CString(world, u8"Hello ☃!");

	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars();

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({
				UnicodeChar('H'),
				UnicodeChar('e'),
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o'),
				UnicodeChar(' '),
				UnicodeChar(0x2603),
				UnicodeChar('!')
		}));
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(0, 8);

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({
				UnicodeChar('H'),
				UnicodeChar('e'),
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o'),
				UnicodeChar(' '),
				UnicodeChar(0x2603),
				UnicodeChar('!')
		}));
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(0, 0);

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({}));
	}

	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(8, 8);

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({}));
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(2);

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o'),
				UnicodeChar(' '),
				UnicodeChar(0x2603),
				UnicodeChar('!')
		}));
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(2, 5);

		ASSERT_TRUE(unicodeChars == std::vector<UnicodeChar>({
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o')
		}));
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(2, 19);
		ASSERT_TRUE(unicodeChars.empty());
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(19);
		ASSERT_TRUE(unicodeChars.empty());
	}
	
	{
		std::vector<UnicodeChar> unicodeChars = helloValue->unicodeChars(19, 24);
		ASSERT_TRUE(unicodeChars.empty());
	}
}

void testToUtf8Bytevector(World &world)
{
	alloc::StringRef helloValue(world, StringCell::fromUtf8CString(world, u8"Hello ☃!"));

	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world);

		ASSERT_EQUAL(byteVectorCell->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorCell->byteArray()->data(), "Hello ☃!", 10), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 0, 8);
		
		ASSERT_EQUAL(byteVectorCell->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorCell->byteArray()->data(), "Hello ☃!", 10), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 2);
		
		ASSERT_EQUAL(byteVectorCell->length(), 8);
		ASSERT_EQUAL(memcmp(byteVectorCell->byteArray()->data(), "llo ☃!", 8), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 2, 5);

		ASSERT_EQUAL(byteVectorCell->length(), 3);
		ASSERT_EQUAL(memcmp(byteVectorCell->byteArray()->data(), "llo", 3), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 2, 19);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 19);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(world, 19, 24);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
}

void testCaseConversion(World &world)
{
	{
		alloc::StringRef mixedCaseAsciiString(world, StringCell::fromUtf8CString(world, u8"Hello, World!"));

		alloc::StringRef lowercaseAsciiString(world, mixedCaseAsciiString->toLowercaseString(world));
		alloc::StringRef uppercaseAsciiString(world, mixedCaseAsciiString->toUppercaseString(world));
		alloc::StringRef caseFoldedAsciiString(world, mixedCaseAsciiString->toCaseFoldedString(world));

		ASSERT_UTF8_EQUAL(lowercaseAsciiString.data(), u8"hello, world!");
		ASSERT_UTF8_EQUAL(uppercaseAsciiString.data(), u8"HELLO, WORLD!");
		ASSERT_UTF8_EQUAL(caseFoldedAsciiString.data(), u8"hello, world!");
	}

	{
		alloc::StringRef mixedCaseUnicodeString(world, StringCell::fromUtf8CString(world, u8"Γεια σας Παγκόσμιο!"));
		
		alloc::StringRef lowercaseUnicodeString(world, mixedCaseUnicodeString->toLowercaseString(world));
		alloc::StringRef uppercaseUnicodeString(world, mixedCaseUnicodeString->toUppercaseString(world));
		alloc::StringRef caseFoldedUnicodeString(world, mixedCaseUnicodeString->toCaseFoldedString(world));
		
		ASSERT_UTF8_EQUAL(lowercaseUnicodeString.data(), u8"γεια σας παγκόσμιο!");
		ASSERT_UTF8_EQUAL(uppercaseUnicodeString.data(), u8"ΓΕΙΑ ΣΑΣ ΠΑΓΚΌΣΜΙΟ!");
		// Note that the final sigma folds to a normal sigma here
		ASSERT_UTF8_EQUAL(caseFoldedUnicodeString.data(), u8"γεια σασ παγκόσμιο!");
	}
	
	{
		alloc::StringRef hanString(world, StringCell::fromUtf8CString(world, u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨"));

		alloc::StringRef lowercaseHanString(world, hanString->toLowercaseString(world));
		alloc::StringRef uppercaseHanString(world, hanString->toUppercaseString(world));
		alloc::StringRef caseFoldedHanString(world, hanString->toCaseFoldedString(world));
		
		ASSERT_UTF8_EQUAL(lowercaseHanString.data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(uppercaseHanString.data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(caseFoldedHanString.data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
	}
	
	{
		alloc::StringRef symbolString(world, StringCell::fromUtf8CString(world, u8"🐉☃☙"));

		alloc::StringRef lowercaseSymbolString(world, symbolString->toLowercaseString(world));
		alloc::StringRef uppercaseSymbolString(world, symbolString->toUppercaseString(world));
		alloc::StringRef caseFoldedSymbolString(world, symbolString->toCaseFoldedString(world));
		
		ASSERT_UTF8_EQUAL(lowercaseSymbolString.data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(uppercaseSymbolString.data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(caseFoldedSymbolString.data(), u8"🐉☃☙");
	}

	{
		alloc::StringRef unusualFoldingString(world, StringCell::fromUtf8CString(world, u8"µϵẛ"));
		
		alloc::StringRef lowercaseFoldingString(world, unusualFoldingString->toLowercaseString(world));
		alloc::StringRef uppercaseFoldingString(world, unusualFoldingString->toUppercaseString(world));
		alloc::StringRef caseFoldedFoldingString(world, unusualFoldingString->toCaseFoldedString(world));

		ASSERT_UTF8_EQUAL(lowercaseFoldingString.data(), u8"µϵẛ");
		ASSERT_UTF8_EQUAL(uppercaseFoldingString.data(), u8"ΜΕṠ");
		ASSERT_UTF8_EQUAL(caseFoldedFoldingString.data(), u8"μεṡ");
	}
}

void testAll(World &world)
{
	testFromUtf8CString(world);
	testFromUtf8StdString(world);
	testFromUtf8Data(world);

	testCompare(world);
	testCharAt(world);
	
	testFromFill(world);
	testFromAppended(world);
	testStringCellBuilder(world);

	testStringCopy(world);

	testSetCharAt(world);
	testFill(world);
	testReplace(world);

	testUnicodeChars(world);

	testToUtf8Bytevector(world);

	testCaseConversion(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);

	return 0;
}
