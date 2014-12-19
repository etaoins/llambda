#include "binding/SymbolCell.h"
#include "binding/StringCell.h"

#include "core/init.h"
#include "core/World.h"
#include "assertions.h"
#include "stubdefinitions.h"

#include "unicode/utf8/InvalidByteSequenceException.h"

#include "alloc/cellref.h"

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
		alloc::StringRef asciiInlineString(world, StringCell::fromUtf8StdString(world, "Hello"));

		SymbolCell *testSymbol = SymbolCell::fromString(world, asciiInlineString);

		ASSERT_EQUAL(testSymbol->byteLength(), 5);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Hello", 5), 0);
	}

	{
		alloc::StringRef unicodeInlineString(world, StringCell::fromUtf8StdString(world, "Hello ☃!"));

		SymbolCell *testSymbol = SymbolCell::fromString(world, unicodeInlineString);

		ASSERT_EQUAL(testSymbol->byteLength(), 10);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Hello ☃!", 10), 0);
	}

	{
		alloc::StringRef asciiHeapString(world, StringCell::fromUtf8StdString(world, "Greetings, unit testers!"));

		SymbolCell *testSymbol = SymbolCell::fromString(world, asciiHeapString);

		ASSERT_EQUAL(testSymbol->byteLength(), 24);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(), "Greetings, unit testers!", 24), 0);
	}

	{
		alloc::StringRef unicodeHeapString(world, StringCell::fromUtf8StdString(world, "Look it's a snowman: ☃!"));

		SymbolCell *testSymbol = SymbolCell::fromString(world, unicodeHeapString);

		ASSERT_EQUAL(testSymbol->byteLength(), 25);
		ASSERT_EQUAL(memcmp(testSymbol->constUtf8Data(),  "Look it's a snowman: ☃!", 25), 0);
	}

	{
		alloc::StringRef asciiOversizeString(world, StringCell::fromFill(world, 65536, UnicodeChar(' ')));

		SymbolCell *testSymbol = SymbolCell::fromString(world, asciiOversizeString);
		ASSERT_NULL(testSymbol);
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
	llcore_init(argc, argv);

	lliby::World::launchWorld(&testAll);

	return 0;
}
