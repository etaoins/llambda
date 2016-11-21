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
#include "binding/VectorCell.h"
#include "binding/BytevectorCell.h"
#include "binding/ProperList.h"
#include "binding/CharCell.h"

#include "unicode/utf8/InvalidByteSequenceException.h"

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

	ASSERT_SYMBOL_PARSE("-i");

	ASSERT_INVALID_PARSE(".");
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

	// Out-of-range
	ASSERT_INVALID_PARSE("9223372036854775808");

	// Invalid for octal
	ASSERT_INVALID_PARSE("#o8");

	// Invalid for decimal
	ASSERT_INVALID_PARSE("#da");

	// Invalid for hex
	ASSERT_INVALID_PARSE("#xg");
	ASSERT_INVALID_PARSE("#x:");
}

void testReals(World &world)
{
	ASSERT_PARSES("0.0", FlonumCell::fromValue(world, 0.0));
	ASSERT_PARSES("33.337", FlonumCell::fromValue(world, 33.337));
	ASSERT_PARSES("-0100.0", FlonumCell::fromValue(world, -100.0));
	ASSERT_PARSES(".25", FlonumCell::fromValue(world, 0.25));
	ASSERT_PARSES("-.125", FlonumCell::fromValue(world, -0.125));

	ASSERT_PARSES("5e-1", FlonumCell::fromValue(world, 0.5));
	ASSERT_PARSES("5e+1", FlonumCell::fromValue(world, 50.0));
	ASSERT_PARSES("2.5e5", FlonumCell::fromValue(world, 250000.0));
	ASSERT_PARSES(".5e0", FlonumCell::fromValue(world, 0.5));
	ASSERT_PARSES("-5e6", FlonumCell::fromValue(world, -5000000.0));

	ASSERT_PARSES("10eat", ExactIntegerCell::fromValue(world, 10));
	ASSERT_PARSES("10+eat", ExactIntegerCell::fromValue(world, 10));

	ASSERT_PARSES("+inf.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-inf.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+INF.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-INF.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+NaN.0", FlonumCell::NaN(world));
	ASSERT_PARSES("-NaN.0", FlonumCell::NaN(world));

	ASSERT_PARSES("+inf.00", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-inf.00", FlonumCell::negativeInfinity(world));
	ASSERT_PARSES("+nan.00", FlonumCell::NaN(world));
	ASSERT_PARSES("-nan.00", FlonumCell::NaN(world));

	ASSERT_INVALID_PARSE(".");
	ASSERT_INVALID_PARSE("#b.");
	ASSERT_INVALID_PARSE("#o.");
	ASSERT_INVALID_PARSE("#d.");
	ASSERT_INVALID_PARSE("#x.");
	ASSERT_INVALID_PARSE("1e9223372036854775808");
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

	// Invalid Unicode code point
	ASSERT_INVALID_PARSE(R"("\x110000;")");
	// Too big for 64bit integers
	ASSERT_INVALID_PARSE(R"("\xFFFFFFFFFFFFFFFFFF;")");
}

void testProperList(World &world)
{
	alloc::SymbolRef helloSymbol(world, SymbolCell::fromUtf8StdString(world, "Hello"));
	alloc::SymbolRef integerSymbol(world, SymbolCell::fromUtf8StdString(world, "integer?"));
	alloc::ExactIntegerRef negativeOne(world, ExactIntegerCell::fromValue(world, -1));
	alloc::FlonumRef plusTwo(world, FlonumCell::fromValue(world, 2.0));

	ProperList<AnyCell> *expectedList = ProperList<AnyCell>::create(world, {BooleanCell::trueInstance(), integerSymbol, helloSymbol, negativeOne, plusTwo});

	ASSERT_PARSES("(#true integer? |Hello| -1 2.0)", expectedList);
}

void testImproperList(World &world)
{
	ASSERT_PARSES("(. #t)", BooleanCell::trueInstance());

	alloc::SymbolRef oneSymbol(world, SymbolCell::fromUtf8StdString(world, "ONE"));
	alloc::FlonumRef plusTwo(world, FlonumCell::fromValue(world, 2.0));
	alloc::FlonumRef plusInf(world, FlonumCell::positiveInfinity(world));

	AnyCell *expectedList = ListElementCell::createList(world, {BooleanCell::falseInstance(), oneSymbol, plusTwo}, plusInf);

   	ASSERT_PARSES("(#false ONE 2.0 . +inf.0)", expectedList);

	alloc::SymbolRef twoSymbol(world, SymbolCell::fromUtf8StdString(world, "TWO"));
	alloc::SymbolRef dotThreeSymbol(world, SymbolCell::fromUtf8StdString(world, ".THREE"));
	expectedList = ProperList<AnyCell>::create(world, {oneSymbol, twoSymbol, dotThreeSymbol});

   	ASSERT_PARSES("(ONE TWO .THREE)", expectedList);

	// List needs to terminate after .
	ASSERT_INVALID_PARSE("(one . two three)");
	// No datum after .
	ASSERT_INVALID_PARSE("(one two three .)");
}

void testSquareProperList(World &world)
{
	alloc::SymbolRef helloSymbol(world, SymbolCell::fromUtf8StdString(world, "Hello"));
	alloc::SymbolRef integerSymbol(world, SymbolCell::fromUtf8StdString(world, "integer?"));
	alloc::ExactIntegerRef negativeOne(world, ExactIntegerCell::fromValue(world, -1));
	alloc::FlonumRef plusTwo(world, FlonumCell::fromValue(world, 2.0));

	ProperList<AnyCell> *expectedList = ProperList<AnyCell>::create(world, {BooleanCell::trueInstance(), integerSymbol, helloSymbol, negativeOne, plusTwo});

	ASSERT_PARSES("[#true integer?\n|Hello| -1 2.0]", expectedList);
}

void testSquareImproperList(World &world)
{
	alloc::SymbolRef oneSymbol(world, SymbolCell::fromUtf8StdString(world, "ONE"));
	alloc::FlonumRef plusTwo(world, FlonumCell::fromValue(world, 2.0));
	alloc::FlonumRef plusInf(world, FlonumCell::positiveInfinity(world));

	AnyCell *expectedList = ListElementCell::createList(world, {BooleanCell::falseInstance(), oneSymbol, plusTwo}, plusInf);

   	ASSERT_PARSES("[#false ONE 2.0 . +inf.0]", expectedList);
}

void testVector(World &world)
{
	alloc::ExactIntegerRef zero(world, ExactIntegerCell::fromValue(world, 0));
	alloc::ExactIntegerRef two(world, ExactIntegerCell::fromValue(world, 2));
	alloc::SymbolRef annaSymbol(world, SymbolCell::fromUtf8StdString(world, "Anna"));

	alloc::AnyRef innerList(world, ProperList<AnyCell>::create(world, {two, two, two, two}));

	VectorCell *expectedVector = VectorCell::fromFill(world, 3, UnitCell::instance());
	expectedVector->setElementAt(0, zero);
	expectedVector->setElementAt(1, innerList);
	expectedVector->setElementAt(2, annaSymbol);

	ASSERT_PARSES("#(0 (2 2 2 2)Anna)", expectedVector);

	expectedVector = VectorCell::fromFill(world, 0, UnitCell::instance());
	ASSERT_PARSES("#()", expectedVector);

	// Unclosed
	ASSERT_INVALID_PARSE("#(bar");
}

void testBytevector(World &world)
{
	std::uint8_t expectedElements[] = {0, 10, 5, 255, 0};

	BytevectorCell *expectedBytevector = BytevectorCell::fromData(world, expectedElements, sizeof(expectedElements));
	ASSERT_PARSES("#u8(+0 10. 5 #xff #d0)", expectedBytevector);

	expectedBytevector = BytevectorCell::fromData(world, expectedElements, 0);
	ASSERT_PARSES("#u8()", expectedBytevector);

	ASSERT_INVALID_PARSE("#u8(1 2");
}

void testUnit(World &world)
{
	ASSERT_PARSES("#!unit", UnitCell::instance());
}

void testComments(World &world)
{
	ProperList<AnyCell> *expectedList = ProperList<AnyCell>::create(world, {BooleanCell::falseInstance(), BooleanCell::trueInstance()});
	ASSERT_PARSES("(#f ; COMMENT\n #t)", expectedList);

	alloc::SymbolRef helloSymbol(world, SymbolCell::fromUtf8StdString(world, "Hello"));
	alloc::SymbolRef jerkSymbol(world, SymbolCell::fromUtf8StdString(world, "jerk"));

	expectedList = ProperList<AnyCell>::create(world, {helloSymbol});
	ASSERT_PARSES("(Hello #;(you jerk))", expectedList);

	expectedList = ProperList<AnyCell>::create(world, {helloSymbol, jerkSymbol});
	ASSERT_PARSES("(Hello #;  you jerk)", expectedList);

	alloc::SymbolRef displaySymbol(world, SymbolCell::fromUtf8StdString(world, "display"));
	alloc::StringRef lolString(world, StringCell::fromUtf8StdString(world, "LOL"));

	const char *multilineTest = R"(
      #| This is a block comment\
         This can be as many lines as it wants
         It can also contain # and |
         It can even contain a #| nested comment |# |#
      (display "LOL")
      #| Make sure we treat this as a separate comment |#
	)";

	expectedList = ProperList<AnyCell>::create(world, {displaySymbol, lolString});
	ASSERT_PARSES(multilineTest, expectedList);
}

void testSymbolShorthand(World &world, std::string shorthand, std::string expansion)
{
	alloc::SymbolRef expansionSymbol(world, SymbolCell::fromUtf8StdString(world, expansion));
	alloc::SymbolRef fooSymbol(world, SymbolCell::fromUtf8StdString(world, "foo"));

	ProperList<AnyCell> *expectedList;

	expectedList = ProperList<AnyCell>::create(world, {expansionSymbol, fooSymbol});
	ASSERT_PARSES(shorthand + "foo", expectedList);

	alloc::ExactIntegerRef exactOne(world, ExactIntegerCell::fromValue(world, 1));
	alloc::ExactIntegerRef exactTwo(world, ExactIntegerCell::fromValue(world, 2));
	alloc::PairRef oneTwoPair(world, PairCell::createInstance(world, exactOne, exactTwo));

	expectedList = ProperList<AnyCell>::create(world, {expansionSymbol, oneTwoPair});
	ASSERT_PARSES(shorthand + " (1 . 2)", expectedList);

	alloc::SymbolRef realPSymbol(world, SymbolCell::fromUtf8StdString(world, "rational?"));
	alloc::FlonumRef inexactOne(world, FlonumCell::fromValue(world, 1.0));
	alloc::StrongRef<ProperList<AnyCell>> realPList(world, ProperList<AnyCell>::create(world, {realPSymbol, inexactOne}));

	expectedList = ProperList<AnyCell>::create(world, {expansionSymbol, realPList});
	ASSERT_PARSES(shorthand + "(rational? 1.0)", expectedList);

	alloc::StrongRef<ProperList<AnyCell>> innerList(world, ProperList<AnyCell>::create(world, {expansionSymbol, BooleanCell::trueInstance()}));

	expectedList = ProperList<AnyCell>::create(world, {expansionSymbol, innerList});
	ASSERT_PARSES(shorthand + shorthand + "#true", expectedList);

	// Unexpected EOF while reading a shorthand
	ASSERT_INVALID_PARSE("'");
}

void testQuotedData(World &world)
{
	testSymbolShorthand(world, "'", "quote");
}

void testQuasiquotedData(World &world)
{
	testSymbolShorthand(world, "`", "quasiquote");
}

void testUnquotedData(World &world)
{
	testSymbolShorthand(world, ",", "unquote");
}

void testSplicingUnquotedData(World &world)
{
	testSymbolShorthand(world, ",@", "unquote-splicing");
}

void testCharacters(World &world)
{
	ASSERT_PARSES(R"(#\alarm)", CharCell::createInstance(world, 0x07));
	ASSERT_PARSES(R"(#\backspace)", CharCell::createInstance(world, 0x08));
	ASSERT_PARSES(R"(#\delete)", CharCell::createInstance(world, 0x7f));
	ASSERT_PARSES(R"(#\escape)", CharCell::createInstance(world, 0x1b));
	ASSERT_PARSES(R"(#\newline)", CharCell::createInstance(world, 0x0a));
	ASSERT_PARSES(R"(#\null)", CharCell::createInstance(world, 0x00));
	ASSERT_PARSES(R"(#\return)", CharCell::createInstance(world, 0x0d));
	ASSERT_PARSES(R"(#\space)", CharCell::createInstance(world, 0x20));
	ASSERT_PARSES(R"(#\tab)", CharCell::createInstance(world, 0x09));

	ASSERT_PARSES(R"(#\a)", CharCell::createInstance(world, 'a'));
	ASSERT_PARSES(R"(#\A)", CharCell::createInstance(world, 'A'));
	ASSERT_PARSES(R"(#\()", CharCell::createInstance(world, '('));
	ASSERT_PARSES(R"(#\ )", CharCell::createInstance(world, ' '));
	ASSERT_PARSES(R"(#\x03BB)", CharCell::createInstance(world, 0x3bb));
	ASSERT_PARSES(R"(#\☃)", CharCell::createInstance(world, 0x2603));

	ASSERT_INVALID_PARSE(R"(#\SPACE)");

	ASSERT_INVALID_PARSE("#");
	ASSERT_INVALID_PARSE(R"(#\)");

	alloc::VectorRef expectedVector(world, VectorCell::fromFill(world, 2, UnitCell::instance()));

	alloc::CharRef oneChar(world, CharCell::createInstance(world, '1'));
	alloc::SymbolRef moreTimeSymbol(world, SymbolCell::fromUtf8StdString(world, "moretime"));

	expectedVector->setElementAt(0, oneChar);
	expectedVector->setElementAt(1, moreTimeSymbol);

	ASSERT_PARSES(R"(#(#\1moretime))", expectedVector);

	ASSERT_INVALID_PARSE(R"(#\x110000)");
}

void testDatumLabels(World &world)
{
	alloc::SymbolRef aSymbol(world, SymbolCell::fromUtf8StdString(world, "a"));
	alloc::SymbolRef bSymbol(world, SymbolCell::fromUtf8StdString(world, "b"));
	alloc::SymbolRef cSymbol(world, SymbolCell::fromUtf8StdString(world, "c"));
	alloc::SymbolRef dSymbol(world, SymbolCell::fromUtf8StdString(world, "d"));
	alloc::SymbolRef eSymbol(world, SymbolCell::fromUtf8StdString(world, "e"));

	alloc::StrongRef<ProperList<SymbolCell>> commonList(world, ProperList<SymbolCell>::create(world, {aSymbol, bSymbol, cSymbol}));
	alloc::StrongRef<ProperList<AnyCell>> nestedList(world, ProperList<AnyCell>::create(world, {dSymbol, eSymbol, commonList}));

    ASSERT_PARSES("(#123=(a b c) . (d e #123#))", PairCell::createInstance(world, commonList, nestedList));

	// Datum label doesn't exist
    ASSERT_INVALID_PARSE("(#123=(a b c) . (d e #456#))");

	// Trying to define a datum label with a huge label numbe
    ASSERT_INVALID_PARSE("#9223372036854775808=(a b c)");

	// Trying to reference a huge datum label
    ASSERT_INVALID_PARSE("#9223372036854775808#");
}

void testErrorRecovery(World &world)
{
	{
		bool caughtException = false;

		// Snowmen aren't valid start characters for data in R7RS
		std::istringstream inputStream(u8"☃123");
		DatumReader reader(world, inputStream);

		try
		{
			reader.parse();
		}
		catch(const MalformedDatumException &e)
		{
			ASSERT_TRUE(e.offset() == 0);
			caughtException = true;
		}

		ASSERT_TRUE(caughtException);

		// Parse the next value
		AnyCell *nextDatum = reader.parse();

		// Make sure it's an integer
		ExactIntegerCell *nextExactInt = cell_cast<ExactIntegerCell>(nextDatum);
		ASSERT_TRUE(nextExactInt != nullptr);

		// Make sure it has the correct value
		ASSERT_EQUAL(nextExactInt->value(), 123);
	}

	{
		bool caughtException = false;

		std::istringstream inputStream("\xFE" "123");
		DatumReader reader(world, inputStream);

		try
		{
			reader.parse();
		}
		catch(const utf8::InvalidHeaderByteException &e)
		{
			caughtException = true;

			ASSERT_EQUAL(e.startOffset(), 0);
			ASSERT_EQUAL(e.endOffset(), 0);
		}

		ASSERT_TRUE(caughtException);

		// Parse the next value
		AnyCell *nextDatum = reader.parse();

		// Make sure it's an integer
		ExactIntegerCell *nextExactInt = cell_cast<ExactIntegerCell>(nextDatum);
		ASSERT_TRUE(nextExactInt != nullptr);

		// Make sure it has the correct value
		ASSERT_EQUAL(nextExactInt->value(), 123);
	}
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
	testProperList(world);
	testImproperList(world);
	testSquareProperList(world);
	testSquareImproperList(world);
	testQuotedData(world);
	testQuasiquotedData(world);
	testUnquotedData(world);
	testSplicingUnquotedData(world);
	testVector(world);
	testBytevector(world);
	testCharacters(world);
	testUnit(world);
	testComments(world);
	testDatumLabels(world);
	testErrorRecovery(world);
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
