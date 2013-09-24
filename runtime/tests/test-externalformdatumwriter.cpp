#include <string>
#include <sstream>

#include "writer/ExternalFormDatumWriter.h"
#include "binding/UnspecificValue.h"
#include "binding/EmptyListValue.h"
#include "binding/BooleanValue.h"
#include "binding/ExactIntegerValue.h"
#include "binding/InexactRationalValue.h"
#include "binding/StringValue.h"
#include "binding/SymbolValue.h"
#include "binding/PairValue.h"
#include "binding/ByteVectorValue.h"
#include "binding/VectorValue.h"
#include "binding/ProcedureValue.h"
#include "binding/CharacterValue.h"

#include "core/init.h"
#include "assertions.h"

namespace
{
using namespace lliby;

void assertForm(const BoxedDatum *datum, std::string expected)
{
	std::ostringstream outputStream;

	ExternalFormDatumWriter writer(outputStream);
	writer.render(datum);

	ASSERT_EQUAL(outputStream.str(), expected);
}

SymbolValue *symbolFor(const char *utf8String)
{
	return StringValue::fromUtf8CString(utf8String)->toSymbol();
}

StringValue *stringFor(const char *utf8String)
{
	return StringValue::fromUtf8CString(utf8String);
}

void testUnspecific()
{
	assertForm(UnspecificValue::instance(), "#!unspecific");
}

void testEmptyList()
{
	assertForm(EmptyListValue::instance(), "()");
}

void testBoolean()
{
	assertForm(BooleanValue::trueInstance(), "#t");
	assertForm(BooleanValue::falseInstance(), "#f");
}

void testExactInteger()
{
	assertForm(ExactIntegerValue::instanceForValue(25), "25");
	assertForm(ExactIntegerValue::instanceForValue(0), "0");
	assertForm(ExactIntegerValue::instanceForValue(-31337), "-31337");
}

void testInexactRational()
{
	assertForm(new InexactRationalValue(0.0), "0.0");

	assertForm(new InexactRationalValue(12.5), "12.5");
	assertForm(new InexactRationalValue(-4.55), "-4.55");

	assertForm(new InexactRationalValue(100.0), "100.0");
	assertForm(new InexactRationalValue(-500.0), "-500.0");

	assertForm(InexactRationalValue::NaN(), "+nan.0");
	assertForm(InexactRationalValue::positiveInfinity(), "+inf.0");
	assertForm(InexactRationalValue::negativeInfinity(), "-inf.0");
}

void testSymbol()
{
	assertForm(symbolFor(u8"Hello"), u8"Hello");
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
	SymbolValue *valueA = symbolFor("A");
	SymbolValue *valueB = symbolFor("B");
	SymbolValue *valueC = symbolFor("C");

	assertForm(PairValue::createProperList({}), "()");
	assertForm(PairValue::createProperList({valueA}), "(A)");
	assertForm(PairValue::createProperList({valueA, valueB}), "(A B)");
	assertForm(PairValue::createProperList({valueA, valueB, valueC}), "(A B C)");

	assertForm(PairValue::createImproperList({valueA, valueB}), "(A . B)");
	assertForm(PairValue::createImproperList({valueA, valueB, valueC}), "(A B . C)");

	// Create a  nested list
	BoxedDatum *innerList = PairValue::createImproperList({valueA, valueB, valueC});
	BoxedDatum *outerList = PairValue::createProperList({valueA, valueB, valueC, innerList});
	assertForm(outerList, "(A B C (A B . C))");
}

void testByteVector()
{
	{
		auto *emptyVector = new ByteVectorValue(nullptr, 0);
		assertForm(emptyVector, "#u8()");
	}

	{
		uint8_t testData[5] = { 100, 101, 202, 203, 204 };
		auto *testVector = new ByteVectorValue(testData, 5); 

		assertForm(testVector, "#u8(100 101 202 203 204)");
	}
}

void testVector()
{
	{
		VectorValue *emptyVector = VectorValue::fromFill(0);
		assertForm(emptyVector, "#()");
	}

	{
		VectorValue *fillVector = VectorValue::fromFill(5);

		for(unsigned int i = 0; i < 5; i++)
		{
			fillVector->setElementAt(i, ExactIntegerValue::instanceForValue(i));
		}

		assertForm(fillVector, "#(0 1 2 3 4)");
	}
}

void testProcedure()
{
	assertForm(new ProcedureValue(nullptr, nullptr), "#!procedure");
}

void testCharacter()
{
	assertForm(new CharacterValue(UnicodeChar(0x07)), "#\\alarm");
	assertForm(new CharacterValue(UnicodeChar(0x08)), "#\\backspace");
	assertForm(new CharacterValue(UnicodeChar(0x7f)), "#\\delete");
	assertForm(new CharacterValue(UnicodeChar(0x1b)), "#\\escape");
	assertForm(new CharacterValue(UnicodeChar(0x0a)), "#\\newline");
	assertForm(new CharacterValue(UnicodeChar(0x00)), "#\\null");
	assertForm(new CharacterValue(UnicodeChar(0x0d)), "#\\return");
	assertForm(new CharacterValue(UnicodeChar(0x20)), "#\\space");
	assertForm(new CharacterValue(UnicodeChar(0x09)), "#\\tab");
	assertForm(new CharacterValue(UnicodeChar('A')), "#\\A");
	assertForm(new CharacterValue(UnicodeChar('a')), "#\\a");
	assertForm(new CharacterValue(UnicodeChar('1')), "#\\1");
	assertForm(new CharacterValue(UnicodeChar(')')), "#\\)");
	assertForm(new CharacterValue(UnicodeChar(0x03bb)), "#\\x3bb");
}

}

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	testUnspecific();
	testEmptyList();
	testBoolean();
	testExactInteger();
	testInexactRational();
	testSymbol();
	testString();
	testPair();
	testByteVector();
	testVector();
	testProcedure();
	testCharacter();
}
