#include <string>
#include <sstream>

#include "core/init.h"
#include "core/World.h"

#include "writer/ExternalFormDatumWriter.h"
#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/PairCell.h"
#include "binding/BytevectorCell.h"
#include "binding/VectorCell.h"
#include "binding/ProcedureCell.h"
#include "binding/CharCell.h"
#include "binding/RecordCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/PortCell.h"

#include "alloc/cellref.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

std::string externalFormFor(const AnyCell *datum)
{
	std::ostringstream outputStream;

	ExternalFormDatumWriter writer(outputStream);
	writer.render(datum);

	return outputStream.str();
}

void assertForm(const AnyCell *datum, std::string expected)
{
	ASSERT_EQUAL(externalFormFor(datum), expected);
}

SymbolCell *symbolFor(World &world, const char *utf8String)
{
	return SymbolCell::fromString(world, StringCell::fromUtf8CString(world, utf8String));
}

StringCell *stringFor(World &world, const char *utf8String)
{
	return StringCell::fromUtf8CString(world, utf8String);
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

void testExactInteger(World &world)
{
	assertForm(ExactIntegerCell::fromValue(world, 25), "25");
	assertForm(ExactIntegerCell::fromValue(world, 0), "0");
	assertForm(ExactIntegerCell::fromValue(world,-31337), "-31337");
}

void testFlonum(World &world)
{
	assertForm(FlonumCell::fromValue(world, 0.0), "0.0");

	assertForm(FlonumCell::fromValue(world, 12.5), "12.5");
	assertForm(FlonumCell::fromValue(world, -4.5), "-4.5");

	assertForm(FlonumCell::fromValue(world, 100.0), "100.0");
	assertForm(FlonumCell::fromValue(world, -500.0), "-500.0");

	assertForm(FlonumCell::NaN(world), "+nan.0");
	assertForm(FlonumCell::positiveInfinity(world), "+inf.0");
	assertForm(FlonumCell::negativeInfinity(world), "-inf.0");
}

void testSymbol(World &world)
{
	assertForm(symbolFor(world, u8"Hello"), u8"Hello");
	assertForm(symbolFor(world, u8"HelloWorldThisRequiresHeapAllocation"), u8"HelloWorldThisRequiresHeapAllocation");
	assertForm(symbolFor(world, u8"λ"), u8"|λ|");
	assertForm(symbolFor(world, u8"Hello, world"), u8"|Hello, world|");
	assertForm(symbolFor(world, u8"Back\\slash"), u8"|Back\\\\slash|");
	assertForm(symbolFor(world, u8"P|pe"), u8"|P\\|pe|");
	assertForm(symbolFor(world, u8"Quo\"te"), u8"|Quo\"te|");
	assertForm(symbolFor(world, u8""), u8"||");
}

void testString(World &world)
{
	assertForm(stringFor(world, u8"Hello"), u8"\"Hello\"");
	assertForm(stringFor(world, u8"λ"), u8"\"λ\"");
	assertForm(stringFor(world, u8"Hello, world"), u8"\"Hello, world\"");
	assertForm(stringFor(world, u8"Hello\nworld"), u8"\"Hello\\nworld\"");
	assertForm(stringFor(world, u8"Hello\bworld"), u8"\"Hello\\bworld\"");
	assertForm(stringFor(world, u8"Hello\"world"), u8"\"Hello\\\"world\"");
	assertForm(stringFor(world, u8""), u8"\"\"");
}

void testPair(World &world)
{
	alloc::SymbolRef valueA(world, symbolFor(world, "A"));
	alloc::SymbolRef valueB(world, symbolFor(world, "B"));
	alloc::SymbolRef valueC(world, symbolFor(world, "C"));

	assertForm(PairCell::createProperList(world, {}), "()");
	assertForm(PairCell::createProperList(world, {valueA}), "(A)");
	assertForm(PairCell::createProperList(world, {valueA, valueB}), "(A B)");
	assertForm(PairCell::createProperList(world, {valueA, valueB, valueC}), "(A B C)");

	assertForm(PairCell::createList(world, {valueA}, valueB), "(A . B)");
	assertForm(PairCell::createList(world, {valueA, valueB}, valueC), "(A B . C)");

	// Create a  nested list
	AnyCell *innerList = PairCell::createList(world, {valueA, valueB}, valueC);
	AnyCell *outerList = PairCell::createProperList(world, {valueA, valueB, valueC, innerList});
	assertForm(outerList, "(A B C (A B . C))");
}

void testBytevector(World &world)
{
	{
		auto *emptyVector = BytevectorCell::fromData(world, nullptr, 0);
		assertForm(emptyVector, "#u8()");
	}

	{
		uint8_t testData[5] = { 100, 101, 202, 203, 204 };
		auto *testVector = BytevectorCell::fromData(world, testData, 5); 

		assertForm(testVector, "#u8(100 101 202 203 204)");
	}
}

void testVector(World &world)
{
	{
		VectorCell *emptyVector = VectorCell::fromFill(world, 0);
		assertForm(emptyVector, "#()");
	}

	{
		alloc::VectorRef fillVector(world, VectorCell::fromFill(world, 5));

		for(unsigned int i = 0; i < 5; i++)
		{
			auto newExactInt = ExactIntegerCell::fromValue(world, i);
			fillVector->setElementAt(i, newExactInt);
		}

		assertForm(fillVector, "#(0 1 2 3 4)");
	}
}

void testProcedure(World &world)
{
	// Outputting of pointers isn't consistent across C++ standard libraries
	// This means our null entry point might be output differently on different 
	// platforms. The entry point output is just for debugging so there's not 
	// point checking it.
	std::string procedureForm = externalFormFor(ProcedureCell::createInstance(world, 0, true, nullptr, nullptr));
	const std::string expectedPrefix("#!procedure(");

	ASSERT_TRUE(procedureForm.compare(0, expectedPrefix.length(), expectedPrefix) == 0);

}

void testCharacter(World &world)
{
	assertForm(CharCell::createInstance(world, UnicodeChar(0x07)), "#\\alarm");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x08)), "#\\backspace");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x7f)), "#\\delete");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x1b)), "#\\escape");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x0a)), "#\\newline");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x00)), "#\\null");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x0d)), "#\\return");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x20)), "#\\space");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x09)), "#\\tab");
	assertForm(CharCell::createInstance(world, UnicodeChar('A')), "#\\A");
	assertForm(CharCell::createInstance(world, UnicodeChar('a')), "#\\a");
	assertForm(CharCell::createInstance(world, UnicodeChar('1')), "#\\1");
	assertForm(CharCell::createInstance(world, UnicodeChar(')')), "#\\)");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x03bb)), "#\\x3bb");
}

void testRecord(World &world)
{
    assertForm(RecordCell::createInstance(world, 0, true, nullptr), "#!record");
}

void testErrorObject(World &world)
{
	alloc::StringRef errorString(world, StringCell::fromUtf8CString(world, u8"Test error"));
	auto errorObj = ErrorObjectCell::createInstance(world, errorString, EmptyListCell::instance());

	assertForm(errorObj, "#!error(Test error)");
}

void testPort(World &world)
{
	auto port = PortCell::createInstance(world, &std::cout, false);
	assertForm(port, "#!port");
}

void testAll(World &world)
{
	testUnit();
	testEmptyList();
	testBoolean();
	testExactInteger(world);
	testFlonum(world);
	testSymbol(world);
	testString(world);
	testPair(world);
	testBytevector(world);
	testVector(world);
	testProcedure(world);
	testCharacter(world);
	testRecord(world);
	testErrorObject(world);
	testPort(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();
	
	lliby::World::launchWorld(&testAll);
}
