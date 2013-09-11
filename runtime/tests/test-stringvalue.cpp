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
		ASSERT_EQUAL(emptyValue->asciiOnlyHint(), true);
		ASSERT_EQUAL(emptyValue->charLength(), 0);
	}

	{
		StringValue *helloValue = StringValue::fromUtf8CString(u8"Hello");

		ASSERT_EQUAL(helloValue->byteLength(), 5);
		ASSERT_EQUAL(helloValue->utf8Data()[0], 'H');
		ASSERT_EQUAL(helloValue->utf8Data()[5], 0);
		ASSERT_EQUAL(helloValue->asciiOnlyHint(), true);
		ASSERT_EQUAL(helloValue->charLength(), 5);
	}
	
	{
		StringValue *highUnicodeValue = StringValue::fromUtf8CString(u8"☃🐉");

		ASSERT_EQUAL(highUnicodeValue->byteLength(), 7);
		ASSERT_EQUAL(highUnicodeValue->utf8Data()[7], 0);
		ASSERT_EQUAL(highUnicodeValue->asciiOnlyHint(), false);
		ASSERT_EQUAL(highUnicodeValue->charLength(), 2);
	}
}

void testCompare()
{
	using namespace lliby;

	StringValue *hello1 = StringValue::fromUtf8CString("Hello");
	StringValue *hello2 = new StringValue(utf8Bytes("Hello"), 5);
	StringValue *world = StringValue::fromUtf8CString("world");
	StringValue *nulledHello1 = new StringValue(utf8Bytes("Hell\0o"), 6, true);
	StringValue *nulledHello2 = new StringValue(utf8Bytes("Hell\0o"), 6, false);
	StringValue *hell = StringValue::fromUtf8CString("Hell");

	// Ensure the same instance is equal to itself
	ASSERT_TRUE(*hello1 == *hello1); 

	// Ensure != works
	ASSERT_FALSE(*hello1 != *hello1); 

	// Ensure different instances with the same content are equal
	// Also checks that the asciiOnlyHint isn't used during comparison 
	ASSERT_TRUE(*hello1 == *hello2);

	// Ensure inequal strings are considered equal
	ASSERT_FALSE(*hello1 == *world);

	// Make sure strings with nulls in them are compared correctly
	ASSERT_TRUE(*nulledHello1 == *nulledHello2);

	// Ensure the comparison doesn't stop on the first NULL
	ASSERT_FALSE(*nulledHello1 == *hell);
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
		ASSERT_EQUAL(emptyAsciiValue->asciiOnlyHint(), true);
		ASSERT_EQUAL(emptyAsciiValue->charLength(), 0);
	}
	
	{
		StringValue *emptyUnicodeValue = StringValue::fromFill(0, 0x02603);

		ASSERT_EQUAL(emptyUnicodeValue->byteLength(), 0);
		ASSERT_EQUAL(emptyUnicodeValue->utf8Data()[0], 0);
		// This is tricky - it's still ASCII only because it doesn't actually
		// contain any characters
		ASSERT_EQUAL(emptyUnicodeValue->asciiOnlyHint(), true);
		ASSERT_EQUAL(emptyUnicodeValue->charLength(), 0);
	}
	
	{
		StringValue *asciiValue = StringValue::fromFill(5, 'H');

		ASSERT_EQUAL(asciiValue->byteLength(), 5);
		ASSERT_EQUAL(asciiValue->utf8Data()[5], 0);
		ASSERT_EQUAL(asciiValue->asciiOnlyHint(), true);
		ASSERT_EQUAL(asciiValue->charLength(), 5);
		ASSERT_EQUAL(asciiValue->charAt(0), 'H');
		ASSERT_EQUAL(asciiValue->charAt(4), 'H');
		ASSERT_EQUAL(asciiValue->charAt(5), StringValue::InvalidChar);
	}
	
	{
		StringValue *unicodeValue = StringValue::fromFill(5, 0x02603);

		ASSERT_EQUAL(unicodeValue->byteLength(), 15);
		ASSERT_EQUAL(unicodeValue->utf8Data()[15], 0);
		ASSERT_EQUAL(unicodeValue->asciiOnlyHint(), false);
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
		ASSERT_EQUAL(emptyValue->asciiOnlyHint(), true);
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
		ASSERT_EQUAL(asciiValue->asciiOnlyHint(), true);
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
		ASSERT_EQUAL(unicodeValue->asciiOnlyHint(), false);
		ASSERT_EQUAL(unicodeValue->charLength(), 7);
		ASSERT_EQUAL(memcmp(unicodeValue->utf8Data(), "Hello ☃", 9), 0);
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

	return 0;
}
