#include <assert.h>

#include "binding/StringValue.h"
#include "core/init.h"
#include "assertions.h"

namespace
{

std::uint8_t* utf8Bytes(const char *str)
{
	return (std::uint8_t*)(str);
}

void testFromUtf8CString()
{
	using namespace lliby;
	
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

void testCompare()
{
	using namespace lliby;

	const StringValue *hello1 = StringValue::fromUtf8CString("Hello");
	const StringValue *hello2 = new StringValue(utf8Bytes("Hello"), 5, 5);
	const StringValue *world = StringValue::fromUtf8CString("world");
	const StringValue *nulledHello1 = new StringValue(utf8Bytes("Hell\0o"), 6, 6);
	const StringValue *nulledHello2 = new StringValue(utf8Bytes("Hell\0o"), 6, 6);
	const StringValue *hell = StringValue::fromUtf8CString("Hell");
	const StringValue *unicodeValue = StringValue::fromUtf8CString(u8"☃🐉");

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

	// Make sure high Unicode code points sort after ASCII
	ASSERT_TRUE(unicodeValue->compare(hello1) > 0);
}

void testCharAt()
{
	using namespace lliby;
		
	{
		StringValue *emptyValue = StringValue::fromUtf8CString(u8"");

		ASSERT_EQUAL(emptyValue->charAt(0), StringValue::InvalidChar);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->charAt(0), 'H');
		ASSERT_EQUAL(helloValue->charAt(4), 'o');
		ASSERT_EQUAL(helloValue->charAt(5), StringValue::InvalidChar);
		ASSERT_EQUAL(helloValue->charAt(1024), StringValue::InvalidChar);
	}
	
	{
		StringValue *highUnicodeValue = StringValue::fromUtf8CString(u8"☃🐉");
		
		ASSERT_EQUAL(highUnicodeValue->charAt(0), 0x02603);
		ASSERT_EQUAL(highUnicodeValue->charAt(1), 0x1F409);
		ASSERT_EQUAL(highUnicodeValue->charAt(2), StringValue::InvalidChar);
		ASSERT_EQUAL(highUnicodeValue->charAt(1024), StringValue::InvalidChar);
	}
}

void testFromFill()
{
	using namespace lliby;
	
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
		ASSERT_EQUAL(asciiValue->charAt(5), StringValue::InvalidChar);
	}
	
	{
		StringValue *unicodeValue = StringValue::fromFill(5, 0x02603);

		ASSERT_EQUAL(unicodeValue->byteLength(), 15);
		ASSERT_EQUAL(unicodeValue->utf8Data()[15], 0);
		ASSERT_EQUAL(unicodeValue->charLength(), 5);
		ASSERT_EQUAL(unicodeValue->charAt(0), 0x02603);
		ASSERT_EQUAL(unicodeValue->charAt(4), 0x02603);
		ASSERT_EQUAL(unicodeValue->charAt(5), StringValue::InvalidChar);
	}
}

void testFromAppended()
{
	using namespace lliby;

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
	using namespace lliby;

	{
		StringValue *emptyValue = StringValue::fromCodePoints(std::list<StringValue::CodePoint>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		const std::list<StringValue::CodePoint> helloPoints = {
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
		const std::list<StringValue::CodePoint> unicodePoints = {
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
	using namespace lliby;
	
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
	using namespace lliby;

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
	using namespace lliby;
	
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
	using namespace lliby;

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
	using namespace lliby;
	StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello ☃!");

	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints();

		ASSERT_TRUE(codePoints == std::list<StringValue::CodePoint>({
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
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(0, 8);

		ASSERT_TRUE(codePoints == std::list<StringValue::CodePoint>({
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
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(0, 0);

		ASSERT_TRUE(codePoints == std::list<StringValue::CodePoint>({}));
	}
	
	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(2);

		ASSERT_TRUE(codePoints == std::list<StringValue::CodePoint>({
				'l',
				'l',
				'o',
				' ',
				0x2603,
				'!'
		}));
	}
	
	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(2, 5);

		ASSERT_TRUE(codePoints == std::list<StringValue::CodePoint>({
				'l',
				'l',
				'o'
		}));
	}
	
	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(2, 19);
		ASSERT_TRUE(codePoints.empty());
	}
	
	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(19);
		ASSERT_TRUE(codePoints.empty());
	}
	
	{
		std::list<StringValue::CodePoint> codePoints = helloValue->codePoints(19, 24);
		ASSERT_TRUE(codePoints.empty());
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testFromUtf8CString();

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

	return 0;
}
