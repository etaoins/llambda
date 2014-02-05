#include <string>
#include <sstream>

#include "writer/ExternalFormDatumWriter.h"
#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/PairCell.h"
#include "binding/BytevectorCell.h"
#include "binding/VectorCell.h"
#include "binding/ProcedureCell.h"
#include "binding/CharacterCell.h"
#include "binding/RecordCell.h"
#include "binding/ErrorObjectCell.h"

#include "core/init.h"
#include "alloc/StrongRef.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

std::string externalFormFor(const DatumCell *datum)
{
	std::ostringstream outputStream;

	ExternalFormDatumWriter writer(outputStream);
	writer.render(datum);

	return outputStream.str();
}

void assertForm(const DatumCell *datum, std::string expected)
{
	ASSERT_EQUAL(externalFormFor(datum), expected);
}

SymbolCell *symbolFor(const char *utf8String)
{
	return StringCell::fromUtf8CString(utf8String)->toSymbol();
}

StringCell *stringFor(const char *utf8String)
{
	return StringCell::fromUtf8CString(utf8String);
}

void testUnit()
{
	assertForm(UnitCell::instance(), "#!unit");
}

void testEmptyList()
{
	assertForm(EmptyListCell::instance(), "()");
}

void testBoolean()
{
	assertForm(BooleanCell::trueInstance(), "#t");
	assertForm(BooleanCell::falseInstance(), "#f");
}

void testExactInteger()
{
	assertForm(ExactIntegerCell::fromValue(25), "25");
	assertForm(ExactIntegerCell::fromValue(0), "0");
	assertForm(ExactIntegerCell::fromValue(-31337), "-31337");
}

void testInexactRational()
{
	assertForm(InexactRationalCell::fromValue(0.0), "0.0");

	assertForm(InexactRationalCell::fromValue(12.5), "12.5");
	assertForm(InexactRationalCell::fromValue(-4.55), "-4.55");

	assertForm(InexactRationalCell::fromValue(100.0), "100.0");
	assertForm(InexactRationalCell::fromValue(-500.0), "-500.0");

	assertForm(InexactRationalCell::NaN(), "+nan.0");
	assertForm(InexactRationalCell::positiveInfinity(), "+inf.0");
	assertForm(InexactRationalCell::negativeInfinity(), "-inf.0");
}

void testSymbol()
{
	assertForm(symbolFor(u8"Hello"), u8"Hello");
	assertForm(symbolFor(u8"HelloWorldThisRequiresHeapAllocation"), u8"HelloWorldThisRequiresHeapAllocation");
	assertForm(symbolFor(u8"λ"), u8"|λ|");
	assertForm(symbolFor(u8"Hello, world"), u8"|Hello, world|");
	assertForm(symbolFor(u8"Back\\slash"), u8"|Back\\\\slash|");
	assertForm(symbolFor(u8"P|pe"), u8"|P\\|pe|");
	assertForm(symbolFor(u8"Quo\"te"), u8"|Quo\"te|");
	assertForm(symbolFor(u8""), u8"||");
}

void testString()
{
	assertForm(stringFor(u8"Hello"), u8"\"Hello\"");
	assertForm(stringFor(u8"λ"), u8"\"λ\"");
	assertForm(stringFor(u8"Hello, world"), u8"\"Hello, world\"");
	assertForm(stringFor(u8"Hello\nworld"), u8"\"Hello\\nworld\"");
	assertForm(stringFor(u8"Hello\bworld"), u8"\"Hello\\bworld\"");
	assertForm(stringFor(u8"Hello\"world"), u8"\"Hello\\\"world\"");
	assertForm(stringFor(u8""), u8"\"\"");
}

void testPair()
{
	alloc::StrongRef<SymbolCell> valueA = symbolFor("A");
	alloc::StrongRef<SymbolCell> valueB = symbolFor("B");
	alloc::StrongRef<SymbolCell> valueC = symbolFor("C");

	assertForm(PairCell::createProperList({}), "()");
	assertForm(PairCell::createProperList({valueA}), "(A)");
	assertForm(PairCell::createProperList({valueA, valueB}), "(A B)");
	assertForm(PairCell::createProperList({valueA, valueB, valueC}), "(A B C)");

	assertForm(PairCell::createList({valueA}, valueB), "(A . B)");
	assertForm(PairCell::createList({valueA, valueB}, valueC), "(A B . C)");

	// Create a  nested list
	DatumCell *innerList = PairCell::createList({valueA, valueB}, valueC);
	DatumCell *outerList = PairCell::createProperList({valueA, valueB, valueC, innerList});
	assertForm(outerList, "(A B C (A B . C))");
}

void testBytevector()
{
	{
		auto *emptyVector = new BytevectorCell(nullptr, 0);
		assertForm(emptyVector, "#u8()");
	}

	{
		uint8_t testData[5] = { 100, 101, 202, 203, 204 };
		auto *testVector = BytevectorCell::fromUnownedData(testData, 5); 

		assertForm(testVector, "#u8(100 101 202 203 204)");
	}
}

void testVector()
{
	{
		VectorCell *emptyVector = VectorCell::fromFill(0);
		assertForm(emptyVector, "#()");
	}

	{
		alloc::StrongRef<VectorCell> fillVector = VectorCell::fromFill(5);

		for(unsigned int i = 0; i < 5; i++)
		{
			auto newExactInt = ExactIntegerCell::fromValue(i);
			fillVector->setElementAt(i, newExactInt);
		}

		assertForm(fillVector, "#(0 1 2 3 4)");
	}
}

void testProcedure()
{
	// Outputting of pointers isn't consistent across C++ standard libraries
	// This means our null entry point might be output differently on different 
	// platforms. The entry point output is just for debugging so there's not 
	// point checking it.
	std::string procedureForm = externalFormFor(new ProcedureCell(0, true, nullptr, nullptr));
	const std::string expectedPrefix("#!procedure(");

	ASSERT_TRUE(procedureForm.compare(0, expectedPrefix.length(), expectedPrefix) == 0);

}

void testCharacter()
{
	assertForm(new CharacterCell(UnicodeChar(0x07)), "#\\alarm");
	assertForm(new CharacterCell(UnicodeChar(0x08)), "#\\backspace");
	assertForm(new CharacterCell(UnicodeChar(0x7f)), "#\\delete");
	assertForm(new CharacterCell(UnicodeChar(0x1b)), "#\\escape");
	assertForm(new CharacterCell(UnicodeChar(0x0a)), "#\\newline");
	assertForm(new CharacterCell(UnicodeChar(0x00)), "#\\null");
	assertForm(new CharacterCell(UnicodeChar(0x0d)), "#\\return");
	assertForm(new CharacterCell(UnicodeChar(0x20)), "#\\space");
	assertForm(new CharacterCell(UnicodeChar(0x09)), "#\\tab");
	assertForm(new CharacterCell(UnicodeChar('A')), "#\\A");
	assertForm(new CharacterCell(UnicodeChar('a')), "#\\a");
	assertForm(new CharacterCell(UnicodeChar('1')), "#\\1");
	assertForm(new CharacterCell(UnicodeChar(')')), "#\\)");
	assertForm(new CharacterCell(UnicodeChar(0x03bb)), "#\\x3bb");
}

void testRecord()
{
    assertForm(new RecordCell(0, true, nullptr), "#!record");
}

void testErrorObject()
{
	alloc::StrongRef<StringCell> errorString = StringCell::fromUtf8CString(u8"Test error");
	auto errorObj = ErrorObjectCell::createInstance(errorString, EmptyListCell::instance());

	assertForm(errorObj, "#!error(Test error)");
}

}

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	testUnit();
	testEmptyList();
	testBoolean();
	testExactInteger();
	testInexactRational();
	testSymbol();
	testString();
	testPair();
	testBytevector();
	testVector();
	testProcedure();
	testCharacter();
	testRecord();
	testErrorObject();
}
