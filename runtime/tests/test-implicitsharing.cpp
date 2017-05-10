#include <cstring>

#include "binding/BytevectorCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/SharedByteArray.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace lliby
{

class ImplicitSharingTest
{
public:
	static SharedByteArray *sharedByteArrayFor(SymbolCell *symbol)
	{
		ASSERT_FALSE(symbol->dataIsInline());
		return static_cast<HeapSymbolCell*>(symbol)->heapByteArray();
	}

	static SharedByteArray *sharedByteArrayFor(StringCell *string)
	{
		ASSERT_FALSE(string->dataIsInline());
		return static_cast<HeapStringCell*>(string)->heapByteArray();
	}

	static SharedByteArray *sharedByteArrayFor(BytevectorCell *bytevector)
	{
		return bytevector->byteArray();
	}

	static void testAll(World &world)
	{
		auto sourceString = reinterpret_cast<const std::uint8_t*>(u8"Hello world everyone! This needs to be long!");
		const std::size_t sourceLength = 44;

		// Create a source bytevector
		BytevectorCell *origBv = BytevectorCell::fromData(world, sourceString, sourceLength);

		// Create a direct copy
		BytevectorCell *copyBv = origBv->copy(world);
		ASSERT_TRUE(sharedByteArrayFor(origBv) == sharedByteArrayFor(copyBv));

		// Set an byte of the copy
		ASSERT_TRUE(copyBv->setByteAt(0, 4));
		// The sharing should now be broken
		ASSERT_FALSE(sharedByteArrayFor(origBv) == sharedByteArrayFor(copyBv));

		// Create a copy from appending a single bytevector
		BytevectorCell *appendedBv = BytevectorCell::fromAppended(world, {origBv});
		ASSERT_TRUE(sharedByteArrayFor(origBv) == sharedByteArrayFor(appendedBv));

		// Replace part of the byte array
		ASSERT_TRUE(appendedBv->replace(3, origBv, 0, 1));
		// Sharing should now be broken
		ASSERT_FALSE(sharedByteArrayFor(origBv) == sharedByteArrayFor(appendedBv));

		// Create a string from the bytevector
		StringCell *origString = origBv->utf8ToString(world);
		ASSERT_TRUE(sharedByteArrayFor(origBv) == sharedByteArrayFor(origString));

		// Create a string as a copy
		StringCell *copyString = origString->copy(world);
		ASSERT_TRUE(sharedByteArrayFor(origString) == sharedByteArrayFor(copyString));

		// Set a character in the string
		ASSERT_TRUE(copyString->setCharAt(5, UnicodeChar('!')));
		// Sharing should now be broken
		ASSERT_FALSE(sharedByteArrayFor(origString) == sharedByteArrayFor(copyString));

		// Create a string from appending a single string
		StringCell *appendedString = StringCell::fromAppended(world, {origString});
		ASSERT_TRUE(sharedByteArrayFor(origString) == sharedByteArrayFor(appendedString));

		// Fill the string
		ASSERT_TRUE(appendedString->fill(UnicodeChar(4)));
		// Sharing should now be broken
		ASSERT_FALSE(sharedByteArrayFor(origString) == sharedByteArrayFor(appendedString));

		// Create a symbol from the appended string
		SymbolCell *symbol = SymbolCell::fromString(world, appendedString);
		ASSERT_TRUE(sharedByteArrayFor(appendedString) == sharedByteArrayFor(symbol));

		// Writing to the string again should break sharing
		// Symbols are immutable so breaking cannot happen from the symbol side
		appendedString->replace(1, origString, 0, 1);
		ASSERT_FALSE(sharedByteArrayFor(appendedString) == sharedByteArrayFor(symbol));

		//
		// Test a grand tour of string ->  symbol -> string -> bytevector -> string
		//
		StringCell *firstString = StringCell::fromUtf8StdString(world, u8"Hello world everyone! This is very long!");
		SymbolCell *firstSymbol = SymbolCell::fromString(world, firstString);
		StringCell *secondString = StringCell::fromSymbol(world, firstSymbol);
		BytevectorCell *firstBv = secondString->toUtf8Bytevector(world);
		StringCell *thirdString = firstBv->utf8ToString(world);

		ASSERT_TRUE(sharedByteArrayFor(firstString) == sharedByteArrayFor(thirdString));
	}
};

}

int main(int argc, char *argv[])
{
	llcore_run(lliby::ImplicitSharingTest::testAll, argc, argv);
}

