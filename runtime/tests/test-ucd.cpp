#include "ucd/ucd.h"

#include "core/init.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

std::ostream& operator<<(std::ostream &stream, const lliby::UnicodeChar &unicodeChar)
{
	stream << std::hex << unicodeChar.codePoint();
	return stream;
}

void testToUpper()
{
	// Lowercase a => uppercase A
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x61)), UnicodeChar(0x41));
	// Lowercase z => uppercase Z
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x7a)), UnicodeChar(0x5a));

	// Greek alpha => uppercase
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x3b1)), UnicodeChar(0x391));
	// Greek omega => uppercase
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x3c9)), UnicodeChar(0x3a9));

	// Cyrillic small a => uppercase
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x430)), UnicodeChar(0x410));
	// Cyrillic small psi => uppercase
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x471)), UnicodeChar(0x470));

	// Number 1 should be left alone
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x31)), UnicodeChar(0x31));

	// The snow man should be left alone
	ASSERT_EQUAL(ucd::toUppercase(UnicodeChar(0x2603)), UnicodeChar(0x2603));
}

void testToLower()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x41)), UnicodeChar(0x61));
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x5a)), UnicodeChar(0x7a));

	// Greek alpha => lowercase
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x391)), UnicodeChar(0x3b1));
	// Greek omega => lowercase
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x3a9)), UnicodeChar(0x3c9));

	// Cyrillic small a => lowercase
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x410)), UnicodeChar(0x430));
	// Cyrillic small psi => lowercase
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x470)), UnicodeChar(0x471));

	// Number 1 should be left alone
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x31)), UnicodeChar(0x31));

	// The snow man should be left alone
	ASSERT_EQUAL(ucd::toLowercase(UnicodeChar(0x2603)), UnicodeChar(0x2603));
}

void testToFolded()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(ucd::toCaseFolded(UnicodeChar(0x41)), UnicodeChar(0x61));
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(ucd::toCaseFolded(UnicodeChar(0x5a)), UnicodeChar(0x7a));

	// Greek alpha => lowercase
	ASSERT_EQUAL(ucd::toCaseFolded(UnicodeChar(0x391)), UnicodeChar(0x3b1));
	// Greek omega => lowercase
	ASSERT_EQUAL(ucd::toCaseFolded(UnicodeChar(0x3a9)), UnicodeChar(0x3c9));

	// Small letter long s => lowercase S
	// This is a difference between folding and toLower
	ASSERT_EQUAL(ucd::toCaseFolded(UnicodeChar(0x017F)), UnicodeChar(0x73))
}

void testNumeric()
{
	// Latin 1
	ASSERT_TRUE(ucd::isNumericDigit(UnicodeChar(0x31)));
	ASSERT_EQUAL(ucd::digitValue(UnicodeChar(0x31)), 1);

	// Latin 9
	ASSERT_TRUE(ucd::isNumericDigit(UnicodeChar(0x39)));
	ASSERT_EQUAL(ucd::digitValue(UnicodeChar(0x39)), 9);

	// Arabic-Indic 0
	ASSERT_TRUE(ucd::isNumericDigit(UnicodeChar(0x0660)));
	ASSERT_EQUAL(ucd::digitValue(UnicodeChar(0x0660)), 0);

	// Arabic-Indic 9
	ASSERT_TRUE(ucd::isNumericDigit(UnicodeChar(0x0669)));
	ASSERT_EQUAL(ucd::digitValue(UnicodeChar(0x0669)), 9);

	// Lowercase a is not a digit
	ASSERT_FALSE(ucd::isNumericDigit(UnicodeChar(0x61)));
	ASSERT_EQUAL(ucd::digitValue(UnicodeChar(0x61)), ucd::InvalidDigitValue);
}

void testUnicodePredicates()
{
	{
		// Uppercase A (ASCII Lu)
		const UnicodeChar uppercaseA(0x41);

		ASSERT_TRUE(ucd::isUppercase(uppercaseA));
		ASSERT_FALSE(ucd::isLowercase(uppercaseA));
		ASSERT_TRUE(ucd::isAlphabetic(uppercaseA));
		ASSERT_FALSE(ucd::isWhitespace(uppercaseA));
	}

	{
		// Lowercase A (ASCII Ll)
		const UnicodeChar lowercaseA(0x61);

		ASSERT_FALSE(ucd::isUppercase(lowercaseA));
		ASSERT_TRUE(ucd::isLowercase(lowercaseA));
		ASSERT_TRUE(ucd::isAlphabetic(lowercaseA));
		ASSERT_FALSE(ucd::isWhitespace(lowercaseA));
	}

	{
		// Latin digit 1 (ASCII Nd)
		const UnicodeChar latinDigit1(0x31);

		ASSERT_FALSE(ucd::isUppercase(latinDigit1));
		ASSERT_FALSE(ucd::isLowercase(latinDigit1));
		ASSERT_FALSE(ucd::isAlphabetic(latinDigit1));
		ASSERT_FALSE(ucd::isWhitespace(latinDigit1));
	}

	{
		// Space (ASCII Zs)
		const UnicodeChar space(0x20);

		ASSERT_FALSE(ucd::isUppercase(space));
		ASSERT_FALSE(ucd::isLowercase(space));
		ASSERT_FALSE(ucd::isAlphabetic(space));
		ASSERT_TRUE(ucd::isWhitespace(space));
	}

	{
		// Uppercase alpha (non-ASCII Lu)
		const UnicodeChar uppercaseAlpha(0x391);

		ASSERT_TRUE(ucd::isUppercase(uppercaseAlpha));
		ASSERT_FALSE(ucd::isLowercase(uppercaseAlpha));
		ASSERT_TRUE(ucd::isAlphabetic(uppercaseAlpha));
		ASSERT_FALSE(ucd::isWhitespace(uppercaseAlpha));
	}

	{
		// Lowercase alpha (non-ASCII Ll)
		const UnicodeChar lowercaseAlpha(0x3b1);

		ASSERT_FALSE(ucd::isUppercase(lowercaseAlpha));
		ASSERT_TRUE(ucd::isLowercase(lowercaseAlpha));
		ASSERT_TRUE(ucd::isAlphabetic(lowercaseAlpha));
		ASSERT_FALSE(ucd::isWhitespace(lowercaseAlpha));
	}

	{
		// Arabic-Indic 9 (non-ASCII Nd)
		const UnicodeChar arabicIndic9(0x669);

		ASSERT_FALSE(ucd::isUppercase(arabicIndic9));
		ASSERT_FALSE(ucd::isLowercase(arabicIndic9));
		ASSERT_FALSE(ucd::isAlphabetic(arabicIndic9));
		ASSERT_FALSE(ucd::isWhitespace(arabicIndic9));
	}

	{
		// Punctuation space (non-ASCII Zs)
		const UnicodeChar punctuationSpace(0x2008);

		ASSERT_FALSE(ucd::isUppercase(punctuationSpace));
		ASSERT_FALSE(ucd::isLowercase(punctuationSpace));
		ASSERT_FALSE(ucd::isAlphabetic(punctuationSpace));
		ASSERT_TRUE(ucd::isWhitespace(punctuationSpace));
	}

	{
		// Roman numeral M (Other_Uppercase)
		const UnicodeChar romanNumeralM(0x216F);

		ASSERT_TRUE(ucd::isUppercase(romanNumeralM));
		ASSERT_FALSE(ucd::isLowercase(romanNumeralM));
		ASSERT_TRUE(ucd::isAlphabetic(romanNumeralM));
		ASSERT_FALSE(ucd::isWhitespace(romanNumeralM));
	}

	{
		// Feminine ordinal indicator (Other_Lowercase)
		const UnicodeChar feminineOrdinalIndicator(0xAA);

		ASSERT_FALSE(ucd::isUppercase(feminineOrdinalIndicator));
		ASSERT_TRUE(ucd::isLowercase(feminineOrdinalIndicator));
		ASSERT_TRUE(ucd::isAlphabetic(feminineOrdinalIndicator));
		ASSERT_FALSE(ucd::isWhitespace(feminineOrdinalIndicator));
	}

	{
		// Paragraph separator (non-ASCII Zp)
		const UnicodeChar paragraphSeparator(0x2029);

		ASSERT_FALSE(ucd::isUppercase(paragraphSeparator));
		ASSERT_FALSE(ucd::isLowercase(paragraphSeparator));
		ASSERT_FALSE(ucd::isAlphabetic(paragraphSeparator));
		ASSERT_TRUE(ucd::isWhitespace(paragraphSeparator));
	}

	{
		// Dragon (non-ASCII So)
		const UnicodeChar dragon(0x1F409);

		ASSERT_FALSE(ucd::isUppercase(dragon));
		ASSERT_FALSE(ucd::isLowercase(dragon));
		ASSERT_FALSE(ucd::isAlphabetic(dragon));
		ASSERT_FALSE(ucd::isWhitespace(dragon));
	}

	{
		// Han 'castrate; eunuch' (Han Lo)
		const UnicodeChar castrateEunuch(0x9609);

		ASSERT_FALSE(ucd::isUppercase(castrateEunuch));
		ASSERT_FALSE(ucd::isLowercase(castrateEunuch));
		ASSERT_TRUE(ucd::isAlphabetic(castrateEunuch));
		ASSERT_FALSE(ucd::isWhitespace(castrateEunuch));
	}
}

void testCompare()
{
	const UnicodeChar smallA('a');
	const UnicodeChar uppercaseA('A');

	const UnicodeChar smallAlpha(0x3b1);
	const UnicodeChar capitalAlpha(0x391);

	const UnicodeChar snowman(0x2603);
	const UnicodeChar dragon(0x1f409);

	const UnicodeChar smallLongS(0x017F);
	const UnicodeChar smallS(0x73);

	ASSERT_TRUE(smallA.compare(uppercaseA) > 0);
	ASSERT_TRUE(uppercaseA.compare(smallA) < 0);
	ASSERT_TRUE(uppercaseA.compare(smallA, ucd::toCaseFolded) == 0);

	ASSERT_TRUE(smallAlpha.compare(capitalAlpha) > 0);
	ASSERT_TRUE(capitalAlpha.compare(smallAlpha) < 0);
	ASSERT_TRUE(capitalAlpha.compare(smallAlpha, ucd::toCaseFolded) == 0);

	ASSERT_TRUE(dragon.compare(snowman) > 0);
	ASSERT_TRUE(snowman.compare(dragon) < 0);
	ASSERT_FALSE(snowman.compare(dragon, ucd::toCaseFolded) == 0);

	ASSERT_TRUE(capitalAlpha.compare(smallA) > 0);
	ASSERT_TRUE(smallA.compare(capitalAlpha) < 0);
	ASSERT_FALSE(smallA.compare(capitalAlpha, ucd::toCaseFolded) == 0);

	ASSERT_TRUE(smallS.compare(smallLongS, ucd::toCaseFolded) == 0);
	ASSERT_FALSE(smallS.compare(smallLongS) == 0);
}

void testAll(World &)
{
	testToUpper();
	testToLower();
	testToFolded();
	testNumeric();

	testUnicodePredicates();

	testCompare();
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
