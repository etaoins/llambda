#include <sstream>

#include "core/init.h"
#include "core/World.h"

#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"
#include "binding/SymbolCell.h"
#include "binding/StringCell.h"
#include "binding/UnitCell.h"

#include "reader/DatumReader.h"
#include "reader/ReadErrorException.h"
#include "writer/ExternalFormDatumWriter.h"

#include "alloc/cellref.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

#define ASSERT_PARSES(datumString, expectedRaw) \
{ \
	std::istringstream inputStream(datumString); \
	DatumReader reader(world, inputStream); \
	alloc::StrongRef<AnyCell> expected(world, expectedRaw); \
	\
	AnyCell *actual; \
	\
	try \
	{ \
		actual = reader.parse(); \
	} \
	catch(const ReadErrorException &e) \
	{ \
		ExternalFormDatumWriter writer(std::cerr); \
		std::cerr << "\"" << datumString << "\" did not parse as expected value \""; \
		writer.render(expected); \
		std::cerr << "\"; instead raised parse exception \""; \
		std::cerr << e.message(); \
		std::cerr << "\" at line " << std::dec << __LINE__ << std::endl; \
		\
		exit(-1); \
	} \
	\
	if (!actual->isEqual(expected)) \
	{ \
		ExternalFormDatumWriter writer(std::cerr); \
		std::cerr << "\"" << datumString << "\" did not parse as expected value \""; \
		writer.render(expected); \
		std::cerr << "\"; instead parsed as \""; \
		writer.render(actual); \
		std::cerr << "\" at line " << std::dec << __LINE__ << std::endl; \
		\
		exit(-1); \
	} \
}

#define ASSERT_STRING_PARSE(input, expected) \
	ASSERT_PARSES("\"" input "\"", StringCell::fromUtf8StdString(world, expected));

#define ASSERT_SYMBOL_PARSE(input) \
	ASSERT_PARSES(input, SymbolCell::fromUtf8StdString(world, input));

#define ASSERT_INVALID_PARSE(datumString) \
{ \
	std::istringstream inputStream(datumString); \
	DatumReader reader(world, inputStream); \
	\
	try \
	{ \
		AnyCell *actual = reader.parse(); \
		\
		ExternalFormDatumWriter writer(std::cerr); \
		std::cerr << "\"" << datumString << "\" did not raise a parse exception "; \
		std::cerr << "instead parsed as \""; \
		writer.render(actual); \
		std::cerr << "\" at line " << std::dec << __LINE__ << std::endl; \
		\
		exit(-1); \
	} \
	catch(const ReadErrorException &) \
	{ \
	} \
}

void testEmptyInput(World &world)
{
	ASSERT_PARSES("", EofObjectCell::instance());
}

void testBooleans(World &world)
{
	ASSERT_PARSES("#t", BooleanCell::trueInstance());
	ASSERT_PARSES("#true", BooleanCell::trueInstance());

	ASSERT_PARSES("#f", BooleanCell::falseInstance());
	ASSERT_PARSES("#false", BooleanCell::falseInstance());
}

void testSymbols(World &world)
{
	ASSERT_SYMBOL_PARSE("HELLO");
	ASSERT_SYMBOL_PARSE("HELLO123");
	ASSERT_SYMBOL_PARSE("predicate?");
	ASSERT_SYMBOL_PARSE("!$%&*+-./:<=>?@^_");
	ASSERT_SYMBOL_PARSE("from->to");
	ASSERT_SYMBOL_PARSE("...");

	// These also have a special meaning with reals
	ASSERT_SYMBOL_PARSE("+");
	ASSERT_SYMBOL_PARSE("+foo");
	ASSERT_SYMBOL_PARSE("-");
	ASSERT_SYMBOL_PARSE("-bar");
}

void testEnclosedSymbols(World &world)
{
	 ASSERT_PARSES(R"(|Hello, world!|)", SymbolCell::fromUtf8StdString(world, "Hello, world!"));
	 ASSERT_PARSES(R"(|\"|)", SymbolCell::fromUtf8StdString(world, "\""));
	 ASSERT_PARSES(R"(|\||)", SymbolCell::fromUtf8StdString(world, "|"));
	 ASSERT_PARSES(R"(|two\x20;words|)", SymbolCell::fromUtf8StdString(world, "two words"));
	 ASSERT_PARSES(R"(||)", SymbolCell::fromUtf8StdString(world, ""));
	 ASSERT_PARSES(R"(|\t\t|)", SymbolCell::fromUtf8StdString(world, "\t\t"));

	ASSERT_INVALID_PARSE("|foo");
}

void testIntegers(World &world)
{
	ASSERT_PARSES("0", ExactIntegerCell::fromValue(world, 0));
	ASSERT_PARSES("000", ExactIntegerCell::fromValue(world, 0));
	ASSERT_PARSES("1000", ExactIntegerCell::fromValue(world, 1000));
	ASSERT_PARSES("-1000", ExactIntegerCell::fromValue(world, -1000));

	ASSERT_PARSES("3.", ExactIntegerCell::fromValue(world, 3));

	ASSERT_PARSES("#b111", ExactIntegerCell::fromValue(world, 7));
	ASSERT_PARSES("#B-1000", ExactIntegerCell::fromValue(world, -8));

	ASSERT_PARSES("#O1234", ExactIntegerCell::fromValue(world, 668));
	ASSERT_PARSES("#o-010", ExactIntegerCell::fromValue(world, -8));

	ASSERT_PARSES("#D1234", ExactIntegerCell::fromValue(world, 1234));
	ASSERT_PARSES("#d-010", ExactIntegerCell::fromValue(world, -10));

	ASSERT_PARSES("#Xdead", ExactIntegerCell::fromValue(world, 57005));
	ASSERT_PARSES("#x-b00b5", ExactIntegerCell::fromValue(world, -721077));

	ASSERT_PARSES("9007199254740993", ExactIntegerCell::fromValue(world, 9007199254740993LL));
}

void testReals(World &world)
{
	ASSERT_PARSES("0.0", FlonumCell::fromValue(world, 0.0));
	ASSERT_PARSES("33.337", FlonumCell::fromValue(world, 33.337));
	ASSERT_PARSES("-0100.0", FlonumCell::fromValue(world, -100.0));

	ASSERT_PARSES("2/5", FlonumCell::fromValue(world, 0.4));
	ASSERT_PARSES("+20/50", FlonumCell::fromValue(world, 0.4));
	ASSERT_PARSES("-20/2", FlonumCell::fromValue(world, -10.0));

	ASSERT_PARSES("+inf.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-inf.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+INF.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-INF.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+NaN.0", FlonumCell::NaN(world));
	ASSERT_PARSES("-NaN.0", FlonumCell::NaN(world));
}

void testStrings(World &world)
{
	ASSERT_STRING_PARSE("", "");
	ASSERT_STRING_PARSE(R"(Hello, world!)", "Hello, world!");
	ASSERT_STRING_PARSE(R"(Hello\"World)", "Hello\"World");
	ASSERT_STRING_PARSE(R"(Hello\\World)", "Hello\\World");
	ASSERT_STRING_PARSE(R"(Hello\|World)", "Hello|World");
	ASSERT_STRING_PARSE(R"(Tab\t)", "Tab\t");
	ASSERT_STRING_PARSE(R"(\nnewline)", "\nnewline");
	ASSERT_STRING_PARSE(R"(carriage: \r)", "carriage: \r");
	ASSERT_STRING_PARSE(R"(Space\x20;Bar)", "Space Bar");
	ASSERT_STRING_PARSE(R"(l\x03BB;)", "l\u03bb");
	ASSERT_STRING_PARSE(R"(\x0;null!)", std::string("\u0000null!", 6));
	ASSERT_STRING_PARSE(R"(The word \"recursion\" has many meanings.)", R"(The word "recursion" has many meanings.)");
	ASSERT_STRING_PARSE("Bare\nnewline", "Bare\nnewline");
	ASSERT_STRING_PARSE("Here's text \\\n    containing just one line""", """Here's text containing just one line""");

	ASSERT_INVALID_PARSE("\"open string");
}

void testUnit(World &world)
{
	ASSERT_PARSES("#!unit", UnitCell::instance());
}

void testAll(World &world)
{
	testEmptyInput(world);
	testBooleans(world);
	testSymbols(world);
	testEnclosedSymbols(world);
	testIntegers(world);
	testReals(world);
	testStrings(world);
	testUnit(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
