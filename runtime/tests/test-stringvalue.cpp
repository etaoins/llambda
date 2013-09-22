#include <string.h>

#include "binding/StringValue.h"
#include "binding/ByteVectorValue.h"

#include "core/init.h"
#include "assertions.h"

namespace
{
using namespace lliby;

std::uint8_t* utf8Bytes(const char *str)
{
	return (std::uint8_t*)(str);
}

void testFromUtf8CString()
{
	{
		StringValue *emptyValue = StringValue::fromUtf8CString(u8"");

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->utf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->charLength(), 5);
	}
	
	{
		StringValue *highUnicodeValue = StringValue::fromUtf8CString(u8"☃🐉");

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->utf8Data()[7], 0);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}
}

void testFromUtf8Data()
{
	{
		StringValue *emptyValue = StringValue::fromUtf8Data(nullptr, 0);

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		// Intentionally include NULL as a char to make sure we're NULL safe
		auto helloBytes = reinterpret_cast<const std::uint8_t*>(u8"Hello");
		StringValue *helloValue = StringValue::fromUtf8Data(helloBytes, 6);

		ASSERT_EQUAL(helloValue->byteLength(), 6);
		ASSERT_EQUAL(helloValue->utf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->utf8Data()[6], 0);
		ASSERT_EQUAL(helloValue->charLength(), 6);;
	}
	
	{
		auto highUnicodeBytes = reinterpret_cast<const std::uint8_t*>(u8"☃🐉");
		StringValue *highUnicodeValue = StringValue::fromUtf8Data(highUnicodeBytes, 7);

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->utf8Data()[7], 0);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}
}

void testCompare()
{
	const StringValue *hello1 = StringValue::fromUtf8CString("Hello");
	const StringValue *hello2 = new StringValue(utf8Bytes("Hello"), 5, 5);
	const StringValue *HELLO = StringValue::fromUtf8CString("HELLO");
	const StringValue *world = StringValue::fromUtf8CString("world");
	const StringValue *nulledHello1 = new StringValue(utf8Bytes("Hell\0o"), 6, 6);
	const StringValue *nulledHello2 = new StringValue(utf8Bytes("Hell\0o"), 6, 6);
	const StringValue *hell = StringValue::fromUtf8CString("Hell");
	const StringValue *unicodeValue = StringValue::fromUtf8CString(u8"☃🐉");
	const StringValue *lowercaseUnicode = StringValue::fromUtf8CString(u8"сфmmциist gязэtiйgs!");
	const StringValue *uppercaseUnicode = StringValue::fromUtf8CString(u8"СФMMЦИIST GЯЗЭTIЙGS!");

	ASSERT_TRUE(*hello1 == *hello1); 
	ASSERT_TRUE(hello1->compare(hello1) == 0);
	
	// Ensure != works
	ASSERT_FALSE(*hello1 != *hello1); 

	// Ensure different instances with the same content are equal
	ASSERT_TRUE(*hello1 == *hello2);
	ASSERT_TRUE(hello1->compare(hello2) == 0);

	// Ensure inequal strings are considered equal
	ASSERT_FALSE(*hello1 == *world);
	ASSERT_TRUE(hello1->compare(world) < 0);

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

void testCharAt()
{
	{
		StringValue *emptyValue = StringValue::fromUtf8CString(u8"");

		ASSERT_EQUAL(emptyValue->charAt(0), InvalidCodePoint);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->charAt(0), 'H');
		ASSERT_EQUAL(helloValue->charAt(4), 'o');
		ASSERT_EQUAL(helloValue->charAt(5), InvalidCodePoint);
		ASSERT_EQUAL(helloValue->charAt(1024), InvalidCodePoint);
	}
	
	{
		StringValue *highUnicodeValue = StringValue::fromUtf8CString(u8"☃🐉");
		
		ASSERT_EQUAL(highUnicodeValue->charAt(0), 0x02603);
		ASSERT_EQUAL(highUnicodeValue->charAt(1), 0x1F409);
		ASSERT_EQUAL(highUnicodeValue->charAt(2), InvalidCodePoint);
		ASSERT_EQUAL(highUnicodeValue->charAt(1024), InvalidCodePoint);
	}
}

void testFromFill()
{
	{
		StringValue *emptyAsciiValue = StringValue::fromFill(0, 0);

		ASSERT_EQUAL(emptyAsciiValue->byteLength(), 0);
		ASSERT_EQUAL(emptyAsciiValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyAsciiValue->charLength(), 0);
	}
	
	{
		StringValue *emptyUnicodeValue = StringValue::fromFill(0, 0x02603);

		ASSERT_EQUAL(emptyUnicodeValue->byteLength(), 0);
		ASSERT_EQUAL(emptyUnicodeValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyUnicodeValue->charLength(), 0);
	}
	
	{
		StringValue *asciiValue = StringValue::fromFill(5, 'H');

		ASSERT_EQUAL(asciiValue->byteLength(), 5);
		ASSERT_EQUAL(asciiValue->utf8Data()[5], 0);
		ASSERT_EQUAL(asciiValue->charLength(), 5);
		ASSERT_EQUAL(asciiValue->charAt(0), 'H');
		ASSERT_EQUAL(asciiValue->charAt(4), 'H');
		ASSERT_EQUAL(asciiValue->charAt(5), InvalidCodePoint);
	}
	
	{
		StringValue *unicodeValue = StringValue::fromFill(5, 0x02603);

		ASSERT_EQUAL(unicodeValue->byteLength(), 15);
		ASSERT_EQUAL(unicodeValue->utf8Data()[15], 0);
		ASSERT_EQUAL(unicodeValue->charLength(), 5);
		ASSERT_EQUAL(unicodeValue->charAt(0), 0x02603);
		ASSERT_EQUAL(unicodeValue->charAt(4), 0x02603);
		ASSERT_EQUAL(unicodeValue->charAt(5), InvalidCodePoint);
	}
}

void testFromAppended()
{
	{
		StringValue *emptyValue = StringValue::fromAppended(std::list<const StringValue*>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}
	
	{
		std::list<const StringValue*> appendParts = {
			StringValue::fromUtf8CString(u8"Hello"),
			StringValue::fromUtf8CString(u8" "),
			StringValue::fromUtf8CString(u8"world!")
		};

		StringValue *asciiValue = StringValue::fromAppended(appendParts);
		
		ASSERT_EQUAL(asciiValue->byteLength(), 12);
		ASSERT_EQUAL(asciiValue->utf8Data()[12], 0);
		ASSERT_EQUAL(asciiValue->charLength(), 12);
		ASSERT_EQUAL(memcmp(asciiValue->utf8Data(), u8"Hello world!", 12), 0);
	}
	
	{
		std::list<const StringValue*> appendParts = {
			StringValue::fromUtf8CString(u8"Hello "),
			StringValue::fromUtf8CString(u8"☃")
		};

		StringValue *unicodeValue = StringValue::fromAppended(appendParts);
		
		ASSERT_EQUAL(unicodeValue->byteLength(), 9);
		ASSERT_EQUAL(unicodeValue->utf8Data()[9], 0);
		ASSERT_EQUAL(unicodeValue->charLength(), 7);
		ASSERT_EQUAL(memcmp(unicodeValue->utf8Data(), "Hello ☃", 9), 0);
	}
}

void testFromCodePoints()
{
	{
		StringValue *emptyValue = StringValue::fromCodePoints(std::list<CodePoint>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		const std::list<CodePoint> helloPoints = {
			'H',
			'e',
			'l',
			'l',
			'o'
		};

		StringValue *helloValue = StringValue::fromCodePoints(helloPoints);
		
		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		const std::list<CodePoint> unicodePoints = {
			0x1F409,
			0x02603,
			'!'
		};

		StringValue *unicodeValue = StringValue::fromCodePoints(unicodePoints);
		
		ASSERT_EQUAL(unicodeValue->byteLength(), 8);
		ASSERT_EQUAL(unicodeValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(unicodeValue->utf8Data(), u8"🐉☃!", 9), 0);
	}
}

void testStringCopy()
{
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *helloCopy = helloValue->copy();

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *elloCopy = helloValue->copy(1);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->utf8Data(), u8"ello", 5), 0);
	}
	
	{
		// Make sure there's no boundry condition on the last character
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *elloCopy = helloValue->copy(1, 5);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->utf8Data(), u8"ello", 5), 0);
	}
	
	{
		// Allow empty strings
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *elloCopy = helloValue->copy(0, 0);

		ASSERT_EQUAL(elloCopy->byteLength(), 0);
		ASSERT_EQUAL(elloCopy->charLength(), 0);
	}
	
	{
		// Allow empty from the very end
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *elloCopy = helloValue->copy(5, 5);

		ASSERT_EQUAL(elloCopy->byteLength(), 0);
		ASSERT_EQUAL(elloCopy->charLength(), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *ellCopy = helloValue->copy(1, 4);

		ASSERT_EQUAL(ellCopy->byteLength(), 3);
		ASSERT_EQUAL(ellCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(ellCopy->utf8Data(), u8"ell", 4), 0);
	}
	
	{
		// Off the end
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *invalidCopy = helloValue->copy(0, 16);

		ASSERT_EQUAL(invalidCopy, NULL);
	}
	
	{
		// start > end
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		StringValue *invalidCopy = helloValue->copy(3, 2);

		ASSERT_EQUAL(invalidCopy, NULL);
	}

	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");
		StringValue *japanCopy = japanValue->copy();

		ASSERT_EQUAL(japanCopy->byteLength(), 9);
		ASSERT_EQUAL(japanCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"日本国", 10), 0);
	}
	
	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");
		StringValue *japanCopy = japanValue->copy(1);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本国", 7), 0);
	}
	
	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");
		// Check for the same boundry in Unicode
		StringValue *japanCopy = japanValue->copy(1, 3);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本国", 7), 0);
	}
	
	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");
		StringValue *japanCopy = japanValue->copy(1, 2);

		ASSERT_EQUAL(japanCopy->byteLength(), 3);
		ASSERT_EQUAL(japanCopy->charLength(), 1);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本", 4), 0);
	}
	
	{
		StringValue *mixedValue = StringValue::fromUtf8CString(u8"日Hello国");
		StringValue *helloCopy = mixedValue->copy(1, 6);

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->utf8Data(), u8"Hello", 6), 0);
	}
}

void testSetCharAt()
{
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(0, 'Y'), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Yello", 6), 0);
		
		// Going off the end of the string should fail
		ASSERT_EQUAL(helloValue->setCharAt(5, 'Y'), false);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(1, 0), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(helloValue->charAt(1), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(3, 0x1F409), true);

		ASSERT_EQUAL(helloValue->byteLength(), 8);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hel🐉o", 9), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(5, 'Y'), false);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"日本国");

		ASSERT_EQUAL(helloValue->setCharAt(1, 'O'), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"日O国", 8), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(4, 'Y'), false);
	}
}

void testFill()
{
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill('Y'), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill('Y', 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");
		ASSERT_EQUAL(helloValue->fill('Y', 0, 6), false);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(0x2603), true);

		ASSERT_EQUAL(helloValue->byteLength(), 15);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"☃☃☃☃☃", 16), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(0x2603, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 13);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"H☃☃☃☃", 14), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(0x2603, 1, 4), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"H☃☃☃o", 12), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill('Y', 1, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill('Y'), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill('Y', 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"YYYY☃");
		
		ASSERT_EQUAL(helloValue->fill('Y', 4, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill('Y', 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"☃YYYY", 8), 0);
	}
}

void testReplace()
{
	const StringValue *constWorld = StringValue::fromUtf8CString(u8"world");
	const StringValue *constJapan = StringValue::fromUtf8CString(u8"日本国"); 

	{
		// From R7RS
		StringValue *numbers = StringValue::fromUtf8CString(u8"12345");
		StringValue *letters = StringValue::fromUtf8CString(u8"abcde");

		ASSERT_EQUAL(letters->replace(1, numbers, 0, 2), true);

		ASSERT_EQUAL(letters->byteLength(), 5);
		ASSERT_EQUAL(letters->charLength(), 5);
		ASSERT_EQUAL(memcmp(letters->utf8Data(), u8"a12de", 6), 0);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constWorld), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"world", 6), 0);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"日本国lo", 12), 0);
	}
	
	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");

		ASSERT_EQUAL(japanValue->replace(0, constWorld, 0, 3), true);

		ASSERT_EQUAL(japanValue->byteLength(), 3);
		ASSERT_EQUAL(japanValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanValue->utf8Data(), u8"wor", 4), 0);
	}
	
	{
		StringValue *japanValue = StringValue::fromUtf8CString(u8"日本国");

		// Overruns the string
		ASSERT_EQUAL(japanValue->replace(0, constWorld), false)
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"He日本国", 12), 0);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		// Off the end
		ASSERT_EQUAL(helloValue->replace(3, constJapan), false);
	}
	
	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 9);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"He本国o", 10), 0);
	}
	
	{
		StringValue *complexValue = StringValue::fromUtf8CString(u8"Hello 日本国");

		// We should be able to replace a substring from ourselves
		ASSERT_EQUAL(complexValue->replace(0, complexValue, 6), true);

		ASSERT_EQUAL(complexValue->byteLength(), 21);
		ASSERT_EQUAL(complexValue->charLength(), 9);
		ASSERT_EQUAL(memcmp(complexValue->utf8Data(), u8"日本国lo 日本国", 22), 0);
	}
}

void testCodePoints()
{
	StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello ☃!");

	{
		std::list<CodePoint> codePoints = helloValue->codePoints();

		ASSERT_TRUE(codePoints == std::list<CodePoint>({
				'H',
				'e',
				'l',
				'l',
				'o',
				' ',
				0x2603,
				'!'
		}));
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(0, 8);

		ASSERT_TRUE(codePoints == std::list<CodePoint>({
				'H',
				'e',
				'l',
				'l',
				'o',
				' ',
				0x2603,
				'!'
		}));
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(0, 0);

		ASSERT_TRUE(codePoints == std::list<CodePoint>({}));
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(2);

		ASSERT_TRUE(codePoints == std::list<CodePoint>({
				'l',
				'l',
				'o',
				' ',
				0x2603,
				'!'
		}));
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(2, 5);

		ASSERT_TRUE(codePoints == std::list<CodePoint>({
				'l',
				'l',
				'o'
		}));
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(2, 19);
		ASSERT_TRUE(codePoints.empty());
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(19);
		ASSERT_TRUE(codePoints.empty());
	}
	
	{
		std::list<CodePoint> codePoints = helloValue->codePoints(19, 24);
		ASSERT_TRUE(codePoints.empty());
	}
}

void testToUtf8ByteVector()
{
	StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello ☃!");

	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector();

		ASSERT_EQUAL(byteVectorValue->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorValue->data(), "Hello ☃!", 10), 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(0, 8);
		
		ASSERT_EQUAL(byteVectorValue->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorValue->data(), "Hello ☃!", 10), 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(2);
		
		ASSERT_EQUAL(byteVectorValue->length(), 8);
		ASSERT_EQUAL(memcmp(byteVectorValue->data(), "llo ☃!", 8), 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(2, 5);

		ASSERT_EQUAL(byteVectorValue->length(), 3);
		ASSERT_EQUAL(memcmp(byteVectorValue->data(), "llo", 3), 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(2, 19);
		ASSERT_EQUAL(byteVectorValue, 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(19);
		ASSERT_EQUAL(byteVectorValue, 0);
	}
	
	{
		ByteVectorValue *byteVectorValue = helloValue->toUtf8ByteVector(19, 24);
		ASSERT_EQUAL(byteVectorValue, 0);
	}
}

void testCaseConversion()
{
	{
		const StringValue *mixedCaseAsciiString = StringValue::fromUtf8CString(u8"Hello, World!");

		StringValue *lowercaseAsciiString = mixedCaseAsciiString->toLowercaseString();
		StringValue *uppercaseAsciiString = mixedCaseAsciiString->toUppercaseString();
		StringValue *caseFoldedAsciiString = mixedCaseAsciiString->toCaseFoldedString();

		ASSERT_UTF8_EQUAL(lowercaseAsciiString->utf8Data(), u8"hello, world!");
		ASSERT_UTF8_EQUAL(uppercaseAsciiString->utf8Data(), u8"HELLO, WORLD!");
		ASSERT_UTF8_EQUAL(caseFoldedAsciiString->utf8Data(), u8"hello, world!");
	}

	{
		const StringValue *mixedCaseUnicodeString = StringValue::fromUtf8CString(u8"Γεια σας Παγκόσμιο!");
		
		StringValue *lowercaseUnicodeString = mixedCaseUnicodeString->toLowercaseString();
		StringValue *uppercaseUnicodeString = mixedCaseUnicodeString->toUppercaseString();
		StringValue *caseFoldedUnicodeString = mixedCaseUnicodeString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseUnicodeString->utf8Data(), u8"γεια σας παγκόσμιο!");
		ASSERT_UTF8_EQUAL(uppercaseUnicodeString->utf8Data(), u8"ΓΕΙΑ ΣΑΣ ΠΑΓΚΌΣΜΙΟ!");
		// Note that the final sigma folds to a normal sigma here
		ASSERT_UTF8_EQUAL(caseFoldedUnicodeString->utf8Data(), u8"γεια σασ παγκόσμιο!");
	}
	
	{
		const StringValue *hanString = StringValue::fromUtf8CString(u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");

		StringValue *lowercaseHanString = hanString->toLowercaseString();
		StringValue *uppercaseHanString = hanString->toUppercaseString();
		StringValue *caseFoldedHanString = hanString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(uppercaseHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(caseFoldedHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
	}
	
	{
		const StringValue *symbolString = StringValue::fromUtf8CString(u8"🐉☃☙");

		StringValue *lowercaseSymbolString = symbolString->toLowercaseString();
		StringValue *uppercaseSymbolString = symbolString->toUppercaseString();
		StringValue *caseFoldedSymbolString = symbolString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseSymbolString->utf8Data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(uppercaseSymbolString->utf8Data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(caseFoldedSymbolString->utf8Data(), u8"🐉☃☙");
	}

	{
		const StringValue *unusualFoldingString = StringValue::fromUtf8CString(u8"µϵẛ");
		
		StringValue *lowercaseFoldingString = unusualFoldingString->toLowercaseString();
		StringValue *uppercaseFoldingString = unusualFoldingString->toUppercaseString();
		StringValue *caseFoldedFoldingString = unusualFoldingString->toCaseFoldedString();

		ASSERT_UTF8_EQUAL(lowercaseFoldingString->utf8Data(), u8"µϵẛ");
		ASSERT_UTF8_EQUAL(uppercaseFoldingString->utf8Data(), u8"ΜΕṠ");
		ASSERT_UTF8_EQUAL(caseFoldedFoldingString->utf8Data(), u8"μεṡ");
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testFromUtf8CString();
	testFromUtf8Data();

	testCompare();
	testCharAt();
	
	testFromFill();
	testFromAppended();
	testFromCodePoints();

	testStringCopy();

	testSetCharAt();
	testFill();
	testReplace();

	testCodePoints();

	testToUtf8ByteVector();

	testCaseConversion();

	return 0;
}
