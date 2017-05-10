#include "binding/SymbolCell.h"
#include "binding/StringCell.h"

#include "core/init.h"
#include "core/World.h"
#include "assertions.h"
#include "stubdefinitions.h"

#include "unicode/utf8/InvalidByteSequenceException.h"

namespace
{
using namespace lliby;

void testFromUtf8StdString(World &world)
{
	{
		SymbolCell *emptyValue = SymbolCell::fromUtf8StdString(world, u8"");

		ASSERT_EQUAL(emptyValue->byteLength(), 0);
	}

	{
		SymbolCell *inlineValue = SymbolCell::fromUtf8StdString(world, u8"Hello");

		ASSERT_EQUAL(inlineValue->byteLength(), 5);
		ASSERT_EQUAL(memcmp(inlineValue->constUtf8Data(), "Hello", 5), 0);
	}

	{
		bool caughtException = false;

		try
		{
			// Truncated sequence
			SymbolCell::fromUtf8StdString(world, "H\xE2\x98");
		}
		catch(const utf8::TruncatedInputException &e)
		{
			ASSERT_EQUAL(e.validChars(), 1);
			ASSERT_EQUAL(e.startOffset(), 1);
			ASSERT_EQUAL(e.endOffset(), 2);
			caughtException = true;
		}

		ASSERT_TRUE(caughtException);
	}
}

void testFromString(World &world)
{
	{
		StringCell *asciiInlineString = StringCell::fromUtf8StdString(world, "Hello");

		SymbolCell *testSymbol = SymbolCell::fromString(world, asciiInlineString);

		ASSERT_EQUAL(testSymbol->byteLength(), 5);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Hello", 5), 0);
	}

	{
		StringCell *unicodeInlineString = StringCell::fromUtf8StdString(world, "Hello ☃!");

		SymbolCell *testSymbol = SymbolCell::fromString(world, unicodeInlineString);

		ASSERT_EQUAL(testSymbol->byteLength(), 10);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Hello ☃!", 10), 0);
	}

	{
		StringCell *asciiHeapString = StringCell::fromUtf8StdString(world, "Greetings, fellow unit testers!");

		SymbolCell *testSymbol = SymbolCell::fromString(world, asciiHeapString);

		ASSERT_EQUAL(testSymbol->byteLength(), 31);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Greetings, fellow unit testers!", 31), 0);
	}

	{
		StringCell *unicodeHeapString = StringCell::fromUtf8StdString(world, "Look it's a little snowman: ☃!");

		SymbolCell *testSymbol = SymbolCell::fromString(world, unicodeHeapString);

		ASSERT_EQUAL(testSymbol->byteLength(), 32);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(),  "Look it's a little snowman: ☃!", 32), 0);
	}
}

void testAll(World &world)
{
	testFromUtf8StdString(world);
	testFromString(world);
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
