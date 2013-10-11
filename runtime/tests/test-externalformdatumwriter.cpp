#include <string>
#include <sstream>

#include "writer/ExternalFormDatumWriter.h"
#include "binding/BoxedUnspecific.h"
#include "binding/BoxedEmptyList.h"
#include "binding/BoxedBoolean.h"
#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"
#include "binding/BoxedString.h"
#include "binding/BoxedSymbol.h"
#include "binding/BoxedPair.h"
#include "binding/BoxedBytevector.h"
#include "binding/BoxedVector.h"
#include "binding/BoxedProcedure.h"
#include "binding/BoxedCharacter.h"

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

BoxedSymbol *symbolFor(const char *utf8String)
{
	return BoxedString::fromUtf8CString(utf8String)->toSymbol();
}

BoxedString *stringFor(const char *utf8String)
{
	return BoxedString::fromUtf8CString(utf8String);
}

void testUnspecific()
{
	assertForm(BoxedUnspecific::instance(), "#!unspecific");
}

void testEmptyList()
{
	assertForm(BoxedEmptyList::instance(), "()");
}

void testBoolean()
{
	assertForm(BoxedBoolean::trueInstance(), "#t");
	assertForm(BoxedBoolean::falseInstance(), "#f");
}

void testExactInteger()
{
	assertForm(new BoxedExactInteger(25), "25");
	assertForm(new BoxedExactInteger(0), "0");
	assertForm(new BoxedExactInteger(-31337), "-31337");
}

void testInexactRational()
{
	assertForm(new BoxedInexactRational(0.0), "0.0");

	assertForm(new BoxedInexactRational(12.5), "12.5");
	assertForm(new BoxedInexactRational(-4.55), "-4.55");

	assertForm(new BoxedInexactRational(100.0), "100.0");
	assertForm(new BoxedInexactRational(-500.0), "-500.0");

	assertForm(BoxedInexactRational::NaN(), "+nan.0");
	assertForm(BoxedInexactRational::positiveInfinity(), "+inf.0");
	assertForm(BoxedInexactRational::negativeInfinity(), "-inf.0");
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
	BoxedSymbol *valueA = symbolFor("A");
	BoxedSymbol *valueB = symbolFor("B");
	BoxedSymbol *valueC = symbolFor("C");

	assertForm(BoxedPair::createProperList({}), "()");
	assertForm(BoxedPair::createProperList({valueA}), "(A)");
	assertForm(BoxedPair::createProperList({valueA, valueB}), "(A B)");
	assertForm(BoxedPair::createProperList({valueA, valueB, valueC}), "(A B C)");

	assertForm(BoxedPair::createImproperList({valueA, valueB}), "(A . B)");
	assertForm(BoxedPair::createImproperList({valueA, valueB, valueC}), "(A B . C)");

	// Create a  nested list
	BoxedDatum *innerList = BoxedPair::createImproperList({valueA, valueB, valueC});
	BoxedDatum *outerList = BoxedPair::createProperList({valueA, valueB, valueC, innerList});
	assertForm(outerList, "(A B C (A B . C))");
}

void testBytevector()
{
	{
		auto *emptyVector = new BoxedBytevector(nullptr, 0);
		assertForm(emptyVector, "#u8()");
	}

	{
		uint8_t testData[5] = { 100, 101, 202, 203, 204 };
		auto *testVector = new BoxedBytevector(testData, 5); 

		assertForm(testVector, "#u8(100 101 202 203 204)");
	}
}

void testVector()
{
	{
		BoxedVector *emptyVector = BoxedVector::fromFill(0);
		assertForm(emptyVector, "#()");
	}

	{
		BoxedVector *fillVector = BoxedVector::fromFill(5);

		for(unsigned int i = 0; i < 5; i++)
		{
			fillVector->setElementAt(i, new BoxedExactInteger(i));
		}

		assertForm(fillVector, "#(0 1 2 3 4)");
	}
}

void testProcedure()
{
	assertForm(new BoxedProcedure(0, nullptr, nullptr), "#!procedure");
}

void testCharacter()
{
	assertForm(new BoxedCharacter(UnicodeChar(0x07)), "#\\alarm");
	assertForm(new BoxedCharacter(UnicodeChar(0x08)), "#\\backspace");
	assertForm(new BoxedCharacter(UnicodeChar(0x7f)), "#\\delete");
	assertForm(new BoxedCharacter(UnicodeChar(0x1b)), "#\\escape");
	assertForm(new BoxedCharacter(UnicodeChar(0x0a)), "#\\newline");
	assertForm(new BoxedCharacter(UnicodeChar(0x00)), "#\\null");
	assertForm(new BoxedCharacter(UnicodeChar(0x0d)), "#\\return");
	assertForm(new BoxedCharacter(UnicodeChar(0x20)), "#\\space");
	assertForm(new BoxedCharacter(UnicodeChar(0x09)), "#\\tab");
	assertForm(new BoxedCharacter(UnicodeChar('A')), "#\\A");
	assertForm(new BoxedCharacter(UnicodeChar('a')), "#\\a");
	assertForm(new BoxedCharacter(UnicodeChar('1')), "#\\1");
	assertForm(new BoxedCharacter(UnicodeChar(')')), "#\\)");
	assertForm(new BoxedCharacter(UnicodeChar(0x03bb)), "#\\x3bb");
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
	testBytevector();
	testVector();
	testProcedure();
	testCharacter();
}
