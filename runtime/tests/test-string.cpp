#include <string.h>

#include "binding/StringCell.h"
#include "binding/BytevectorCell.h"

#include "core/init.h"
#include "assertions.h"

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

void testFromUtf8CString()
{
	{
		StringCell *emptyValue = StringCell::fromUtf8CString(u8"");

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->utf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->charLength(), 5);
	}
	
	{
		StringCell *highUnicodeValue = StringCell::fromUtf8CString(u8"☃🐉");

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->utf8Data()[7], 0);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}
}

void testFromUtf8Data()
{
	{
		StringCell *emptyValue = StringCell::fromUtf8Data(nullptr, 0);

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		// Intentionally include NULL as a char to make sure we're NULL safe
		auto helloBytes = reinterpret_cast<const std::uint8_t*>(u8"Hello");
		StringCell *helloValue = StringCell::fromUtf8Data(helloBytes, 6);

		ASSERT_EQUAL(helloValue->byteLength(), 6);
		ASSERT_EQUAL(helloValue->utf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->utf8Data()[6], 0);
		ASSERT_EQUAL(helloValue->charLength(), 6);;
	}
	
	{
		auto highUnicodeBytes = reinterpret_cast<const std::uint8_t*>(u8"☃🐉");
		StringCell *highUnicodeValue = StringCell::fromUtf8Data(highUnicodeBytes, 7);

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->utf8Data()[7], 0);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}
}

void testCompare()
{
	const StringCell *hello1 = StringCell::fromUtf8CString("Hello");
	const StringCell *hello2 = new StringCell(utf8Bytes("Hello"), 5, 5);
	const StringCell *HELLO = StringCell::fromUtf8CString("HELLO");
	const StringCell *world = StringCell::fromUtf8CString("world");
	const StringCell *nulledHello1 = new StringCell(utf8Bytes("Hell\0o"), 6, 6);
	const StringCell *nulledHello2 = new StringCell(utf8Bytes("Hell\0o"), 6, 6);
	const StringCell *hell = StringCell::fromUtf8CString("Hell");
	const StringCell *unicodeValue = StringCell::fromUtf8CString(u8"☃🐉");
	const StringCell *lowercaseUnicode = StringCell::fromUtf8CString(u8"сфmmциist gязэtiйgs!");
	const StringCell *uppercaseUnicode = StringCell::fromUtf8CString(u8"СФMMЦИIST GЯЗЭTIЙGS!");

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
		StringCell *emptyValue = StringCell::fromUtf8CString(u8"");

		ASSERT_FALSE(emptyValue->charAt(0).isValid());
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->charAt(0), UnicodeChar('H'));
		ASSERT_EQUAL(helloValue->charAt(4), UnicodeChar('o'));
		ASSERT_FALSE(helloValue->charAt(5).isValid());
		ASSERT_FALSE(helloValue->charAt(1024).isValid());
	}
	
	{
		StringCell *highUnicodeValue = StringCell::fromUtf8CString(u8"☃🐉");
		
		ASSERT_EQUAL(highUnicodeValue->charAt(0), UnicodeChar(0x02603));
		ASSERT_EQUAL(highUnicodeValue->charAt(1), UnicodeChar(0x1F409));
		ASSERT_FALSE(highUnicodeValue->charAt(2).isValid());
		ASSERT_FALSE(highUnicodeValue->charAt(1024).isValid());
	}
}

void testFromFill()
{
	{
		StringCell *emptyAsciiValue = StringCell::fromFill(0, UnicodeChar(0));

		ASSERT_EQUAL(emptyAsciiValue->byteLength(), 0);
		ASSERT_EQUAL(emptyAsciiValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyAsciiValue->charLength(), 0);
	}
	
	{
		StringCell *emptyUnicodeValue = StringCell::fromFill(0, UnicodeChar(0x02603));

		ASSERT_EQUAL(emptyUnicodeValue->byteLength(), 0);
		ASSERT_EQUAL(emptyUnicodeValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyUnicodeValue->charLength(), 0);
	}
	
	{
		StringCell *asciiValue = StringCell::fromFill(5, UnicodeChar('H'));

		ASSERT_EQUAL(asciiValue->byteLength(), 5);
		ASSERT_EQUAL(asciiValue->utf8Data()[5], 0);
		ASSERT_EQUAL(asciiValue->charLength(), 5);
		ASSERT_EQUAL(asciiValue->charAt(0), UnicodeChar('H'));
		ASSERT_EQUAL(asciiValue->charAt(4), UnicodeChar('H'));
		ASSERT_FALSE(asciiValue->charAt(5).isValid());
	}
	
	{
		StringCell *unicodeValue = StringCell::fromFill(5, UnicodeChar(0x02603));

		ASSERT_EQUAL(unicodeValue->byteLength(), 15);
		ASSERT_EQUAL(unicodeValue->utf8Data()[15], 0);
		ASSERT_EQUAL(unicodeValue->charLength(), 5);
		ASSERT_EQUAL(unicodeValue->charAt(0), UnicodeChar(0x02603));
		ASSERT_EQUAL(unicodeValue->charAt(4), UnicodeChar(0x02603));
		ASSERT_FALSE(unicodeValue->charAt(5).isValid());
	}
}

void testFromAppended()
{
	{
		StringCell *emptyValue = StringCell::fromAppended(std::list<const StringCell*>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}
	
	{
		std::list<const StringCell*> appendParts = {
			StringCell::fromUtf8CString(u8"Hello"),
			StringCell::fromUtf8CString(u8" "),
			StringCell::fromUtf8CString(u8"world!")
		};

		StringCell *asciiValue = StringCell::fromAppended(appendParts);
		
		ASSERT_EQUAL(asciiValue->byteLength(), 12);
		ASSERT_EQUAL(asciiValue->utf8Data()[12], 0);
		ASSERT_EQUAL(asciiValue->charLength(), 12);
		ASSERT_EQUAL(memcmp(asciiValue->utf8Data(), u8"Hello world!", 12), 0);
	}
	
	{
		std::list<const StringCell*> appendParts = {
			StringCell::fromUtf8CString(u8"Hello "),
			StringCell::fromUtf8CString(u8"☃")
		};

		StringCell *unicodeValue = StringCell::fromAppended(appendParts);
		
		ASSERT_EQUAL(unicodeValue->byteLength(), 9);
		ASSERT_EQUAL(unicodeValue->utf8Data()[9], 0);
		ASSERT_EQUAL(unicodeValue->charLength(), 7);
		ASSERT_EQUAL(memcmp(unicodeValue->utf8Data(), "Hello ☃", 9), 0);
	}
}

void testFromUnicodeChars()
{
	{
		StringCell *emptyValue = StringCell::fromUnicodeChars(std::list<UnicodeChar>());
		ASSERT_EQUAL(emptyValue->byteLength(), 0);
		ASSERT_EQUAL(emptyValue->utf8Data()[0], 0);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		const std::list<UnicodeChar> helloPoints = {
			UnicodeChar('H'),
			UnicodeChar('e'),
			UnicodeChar('l'),
			UnicodeChar('l'),
			UnicodeChar('o')
		};

		StringCell *helloValue = StringCell::fromUnicodeChars(helloPoints);
		
		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		const std::list<UnicodeChar> unicodeChars = {
			UnicodeChar(0x1F409),
			UnicodeChar(0x02603),
			UnicodeChar('!')
		};

		StringCell *unicodeValue = StringCell::fromUnicodeChars(unicodeChars);
		
		ASSERT_EQUAL(unicodeValue->byteLength(), 8);
		ASSERT_EQUAL(unicodeValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(unicodeValue->utf8Data(), u8"🐉☃!", 9), 0);
	}
}

void testStringCopy()
{
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *helloCopy = helloValue->copy();

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *elloCopy = helloValue->copy(1);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->utf8Data(), u8"ello", 5), 0);
	}
	
	{
		// Make sure there's no boundry condition on the last character
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *elloCopy = helloValue->copy(1, 5);

		ASSERT_EQUAL(elloCopy->byteLength(), 4);
		ASSERT_EQUAL(elloCopy->charLength(), 4);
		ASSERT_EQUAL(memcmp(elloCopy->utf8Data(), u8"ello", 5), 0);
	}
	
	{
		// Allow empty strings
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *elloCopy = helloValue->copy(0, 0);

		ASSERT_EQUAL(elloCopy->byteLength(), 0);
		ASSERT_EQUAL(elloCopy->charLength(), 0);
	}
	
	{
		// Allow empty from the very end
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *elloCopy = helloValue->copy(5, 5);

		ASSERT_EQUAL(elloCopy->byteLength(), 0);
		ASSERT_EQUAL(elloCopy->charLength(), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *ellCopy = helloValue->copy(1, 4);

		ASSERT_EQUAL(ellCopy->byteLength(), 3);
		ASSERT_EQUAL(ellCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(ellCopy->utf8Data(), u8"ell", 4), 0);
	}
	
	{
		// Off the end
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *invalidCopy = helloValue->copy(0, 16);

		ASSERT_NULL(invalidCopy);
	}
	
	{
		// start > end
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		StringCell *invalidCopy = helloValue->copy(3, 2);

		ASSERT_NULL(invalidCopy);
	}

	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");
		StringCell *japanCopy = japanValue->copy();

		ASSERT_EQUAL(japanCopy->byteLength(), 9);
		ASSERT_EQUAL(japanCopy->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"日本国", 10), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");
		StringCell *japanCopy = japanValue->copy(1);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本国", 7), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");
		// Check for the same boundry in Unicode
		StringCell *japanCopy = japanValue->copy(1, 3);

		ASSERT_EQUAL(japanCopy->byteLength(), 6);
		ASSERT_EQUAL(japanCopy->charLength(), 2);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本国", 7), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");
		StringCell *japanCopy = japanValue->copy(1, 2);

		ASSERT_EQUAL(japanCopy->byteLength(), 3);
		ASSERT_EQUAL(japanCopy->charLength(), 1);
		ASSERT_EQUAL(memcmp(japanCopy->utf8Data(), u8"本", 4), 0);
	}
	
	{
		StringCell *mixedValue = StringCell::fromUtf8CString(u8"日Hello国");
		StringCell *helloCopy = mixedValue->copy(1, 6);

		ASSERT_EQUAL(helloCopy->byteLength(), 5);
		ASSERT_EQUAL(helloCopy->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloCopy->utf8Data(), u8"Hello", 6), 0);
	}
}

void testSetCharAt()
{
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(0, UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Yello", 6), 0);
		
		// Going off the end of the string should fail
		ASSERT_EQUAL(helloValue->setCharAt(5, UnicodeChar('Y')), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(1, UnicodeChar(0)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(helloValue->charAt(1), UnicodeChar(0));
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->setCharAt(3, UnicodeChar(0x1F409)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 8);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hel🐉o", 9), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(5, UnicodeChar('Y')), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"日本国");

		ASSERT_EQUAL(helloValue->setCharAt(1, UnicodeChar('O')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"日O国", 8), 0);
		
		ASSERT_EQUAL(helloValue->setCharAt(4, UnicodeChar('Y')), false);
	}
}

void testFill()
{
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 6), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603)), true);

		ASSERT_EQUAL(helloValue->byteLength(), 15);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"☃☃☃☃☃", 16), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603), 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 13);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"H☃☃☃☃", 14), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar(0x2603), 1, 4), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"H☃☃☃o", 12), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 1, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"Hello", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y')), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 0, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"YYYY☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 4, 5), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"YYYYY", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"☃☃☃☃☃");
		
		ASSERT_EQUAL(helloValue->fill(UnicodeChar('Y'), 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 7);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"☃YYYY", 8), 0);
	}
}

void testReplace()
{
	const StringCell *constWorld = StringCell::fromUtf8CString(u8"world");
	const StringCell *constJapan = StringCell::fromUtf8CString(u8"日本国"); 

	{
		// From R7RS
		StringCell *numbers = StringCell::fromUtf8CString(u8"12345");
		StringCell *letters = StringCell::fromUtf8CString(u8"abcde");

		ASSERT_EQUAL(letters->replace(1, numbers, 0, 2), true);

		ASSERT_EQUAL(letters->byteLength(), 5);
		ASSERT_EQUAL(letters->charLength(), 5);
		ASSERT_EQUAL(memcmp(letters->utf8Data(), u8"a12de", 6), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constWorld), true);

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"world", 6), 0);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(0, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"日本国lo", 12), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");

		ASSERT_EQUAL(japanValue->replace(0, constWorld, 0, 3), true);

		ASSERT_EQUAL(japanValue->byteLength(), 3);
		ASSERT_EQUAL(japanValue->charLength(), 3);
		ASSERT_EQUAL(memcmp(japanValue->utf8Data(), u8"wor", 4), 0);
	}
	
	{
		StringCell *japanValue = StringCell::fromUtf8CString(u8"日本国");

		// Overruns the string
		ASSERT_EQUAL(japanValue->replace(0, constWorld), false)
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan), true);

		ASSERT_EQUAL(helloValue->byteLength(), 11);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"He日本国", 12), 0);
	}

	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		// Off the end
		ASSERT_EQUAL(helloValue->replace(3, constJapan), false);
	}
	
	{
		StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->replace(2, constJapan, 1), true);

		ASSERT_EQUAL(helloValue->byteLength(), 9);
		ASSERT_EQUAL(helloValue->charLength(), 5);
		ASSERT_EQUAL(memcmp(helloValue->utf8Data(), u8"He本国o", 10), 0);
	}
	
	{
		StringCell *complexValue = StringCell::fromUtf8CString(u8"Hello 日本国");

		// We should be able to replace a substring from ourselves
		ASSERT_EQUAL(complexValue->replace(0, complexValue, 6), true);

		ASSERT_EQUAL(complexValue->byteLength(), 21);
		ASSERT_EQUAL(complexValue->charLength(), 9);
		ASSERT_EQUAL(memcmp(complexValue->utf8Data(), u8"日本国lo 日本国", 22), 0);
	}
}

void testUnicodeChars()
{
	StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello ☃!");

	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars();

		ASSERT_TRUE(unicodeChars == std::list<UnicodeChar>({
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
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(0, 8);

		ASSERT_TRUE(unicodeChars == std::list<UnicodeChar>({
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
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(0, 0);

		ASSERT_TRUE(unicodeChars == std::list<UnicodeChar>({}));
	}
	
	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(2);

		ASSERT_TRUE(unicodeChars == std::list<UnicodeChar>({
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o'),
				UnicodeChar(' '),
				UnicodeChar(0x2603),
				UnicodeChar('!')
		}));
	}
	
	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(2, 5);

		ASSERT_TRUE(unicodeChars == std::list<UnicodeChar>({
				UnicodeChar('l'),
				UnicodeChar('l'),
				UnicodeChar('o')
		}));
	}
	
	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(2, 19);
		ASSERT_TRUE(unicodeChars.empty());
	}
	
	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(19);
		ASSERT_TRUE(unicodeChars.empty());
	}
	
	{
		std::list<UnicodeChar> unicodeChars = helloValue->unicodeChars(19, 24);
		ASSERT_TRUE(unicodeChars.empty());
	}
}

void testToUtf8Bytevector()
{
	StringCell *helloValue = StringCell::fromUtf8CString(u8"Hello ☃!");

	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector();

		ASSERT_EQUAL(byteVectorCell->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorCell->data(), "Hello ☃!", 10), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(0, 8);
		
		ASSERT_EQUAL(byteVectorCell->length(), 10);
		ASSERT_EQUAL(memcmp(byteVectorCell->data(), "Hello ☃!", 10), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(2);
		
		ASSERT_EQUAL(byteVectorCell->length(), 8);
		ASSERT_EQUAL(memcmp(byteVectorCell->data(), "llo ☃!", 8), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(2, 5);

		ASSERT_EQUAL(byteVectorCell->length(), 3);
		ASSERT_EQUAL(memcmp(byteVectorCell->data(), "llo", 3), 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(2, 19);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(19);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
	
	{
		BytevectorCell *byteVectorCell = helloValue->toUtf8Bytevector(19, 24);
		ASSERT_EQUAL(byteVectorCell, 0);
	}
}

void testCaseConversion()
{
	{
		const StringCell *mixedCaseAsciiString = StringCell::fromUtf8CString(u8"Hello, World!");

		StringCell *lowercaseAsciiString = mixedCaseAsciiString->toLowercaseString();
		StringCell *uppercaseAsciiString = mixedCaseAsciiString->toUppercaseString();
		StringCell *caseFoldedAsciiString = mixedCaseAsciiString->toCaseFoldedString();

		ASSERT_UTF8_EQUAL(lowercaseAsciiString->utf8Data(), u8"hello, world!");
		ASSERT_UTF8_EQUAL(uppercaseAsciiString->utf8Data(), u8"HELLO, WORLD!");
		ASSERT_UTF8_EQUAL(caseFoldedAsciiString->utf8Data(), u8"hello, world!");
	}

	{
		const StringCell *mixedCaseUnicodeString = StringCell::fromUtf8CString(u8"Γεια σας Παγκόσμιο!");
		
		StringCell *lowercaseUnicodeString = mixedCaseUnicodeString->toLowercaseString();
		StringCell *uppercaseUnicodeString = mixedCaseUnicodeString->toUppercaseString();
		StringCell *caseFoldedUnicodeString = mixedCaseUnicodeString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseUnicodeString->utf8Data(), u8"γεια σας παγκόσμιο!");
		ASSERT_UTF8_EQUAL(uppercaseUnicodeString->utf8Data(), u8"ΓΕΙΑ ΣΑΣ ΠΑΓΚΌΣΜΙΟ!");
		// Note that the final sigma folds to a normal sigma here
		ASSERT_UTF8_EQUAL(caseFoldedUnicodeString->utf8Data(), u8"γεια σασ παγκόσμιο!");
	}
	
	{
		const StringCell *hanString = StringCell::fromUtf8CString(u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");

		StringCell *lowercaseHanString = hanString->toLowercaseString();
		StringCell *uppercaseHanString = hanString->toUppercaseString();
		StringCell *caseFoldedHanString = hanString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(uppercaseHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
		ASSERT_UTF8_EQUAL(caseFoldedHanString->utf8Data(), u8"蒮 駓駗鴀 螒螝螜 咍垀 漊 犨");
	}
	
	{
		const StringCell *symbolString = StringCell::fromUtf8CString(u8"🐉☃☙");

		StringCell *lowercaseSymbolString = symbolString->toLowercaseString();
		StringCell *uppercaseSymbolString = symbolString->toUppercaseString();
		StringCell *caseFoldedSymbolString = symbolString->toCaseFoldedString();
		
		ASSERT_UTF8_EQUAL(lowercaseSymbolString->utf8Data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(uppercaseSymbolString->utf8Data(), u8"🐉☃☙");
		ASSERT_UTF8_EQUAL(caseFoldedSymbolString->utf8Data(), u8"🐉☃☙");
	}

	{
		const StringCell *unusualFoldingString = StringCell::fromUtf8CString(u8"µϵẛ");
		
		StringCell *lowercaseFoldingString = unusualFoldingString->toLowercaseString();
		StringCell *uppercaseFoldingString = unusualFoldingString->toUppercaseString();
		StringCell *caseFoldedFoldingString = unusualFoldingString->toCaseFoldedString();

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
	testFromUnicodeChars();

	testStringCopy();

	testSetCharAt();
	testFill();
	testReplace();

	testUnicodeChars();

	testToUtf8Bytevector();

	testCaseConversion();

	return 0;
}
