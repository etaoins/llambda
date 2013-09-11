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

}

int main(int argc, char *argv[])
{
	lliby_init();

	testFromUtf8CString();
	testCompare();
	testCharAt();

	return 0;
}
