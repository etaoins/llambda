#include "unicodedata/UnicodeData.h"

#include "core/init.h"
#include "assertions.h"

namespace
{

using namespace lliby;

void testToUpper()
{
	// Lowercase a => uppercase A
	ASSERT_EQUAL(UnicodeData::toUppercase(0x61), 0x41);
	// Lowercase z => uppercase Z
	ASSERT_EQUAL(UnicodeData::toUppercase(0x7a), 0x5a);
	
	// Greek alpha => uppercase
	ASSERT_EQUAL(UnicodeData::toUppercase(0x3b1), 0x391);
	// Greek omega => uppercase
	ASSERT_EQUAL(UnicodeData::toUppercase(0x3c9), 0x3a9);

	// Cyrillic small a => uppercase
	ASSERT_EQUAL(UnicodeData::toUppercase(0x430), 0x410);
	// Cyrillic small psi => uppercase
	ASSERT_EQUAL(UnicodeData::toUppercase(0x471), 0x470);

	// Number 1 should be left alone
	ASSERT_EQUAL(UnicodeData::toUppercase(0x31), 0x31);
	
	// The snow man should be left alone
	ASSERT_EQUAL(UnicodeData::toUppercase(0x2603), 0x2603);
}

void testToLower()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(UnicodeData::toLowercase(0x41), 0x61);
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(UnicodeData::toLowercase(0x5a), 0x7a);
	
	// Greek alpha => lowercase 
	ASSERT_EQUAL(UnicodeData::toLowercase(0x391), 0x3b1);
	// Greek omega => lowercase
	ASSERT_EQUAL(UnicodeData::toLowercase(0x3a9), 0x3c9);

	// Cyrillic small a => lowercase
	ASSERT_EQUAL(UnicodeData::toLowercase(0x410), 0x430);
	// Cyrillic small psi => lowercase
	ASSERT_EQUAL(UnicodeData::toLowercase(0x470), 0x471);

	// Number 1 should be left alone
	ASSERT_EQUAL(UnicodeData::toLowercase(0x31), 0x31);
	
	// The snow man should be left alone
	ASSERT_EQUAL(UnicodeData::toLowercase(0x2603), 0x2603);
}

void testToFolded()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(UnicodeData::foldCase(0x41), 0x61);
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(UnicodeData::foldCase(0x5a), 0x7a);
	
	// Greek alpha => lowercase 
	ASSERT_EQUAL(UnicodeData::foldCase(0x391), 0x3b1);
	// Greek omega => lowercase
	ASSERT_EQUAL(UnicodeData::foldCase(0x3a9), 0x3c9);

	// Small letter long s => lowercase S
	// This is a difference between folding and toLower
	ASSERT_EQUAL(UnicodeData::foldCase(0x017F), 0x73)
}

void testToNumericValue()
{
	// Latin 1
	ASSERT_TRUE(UnicodeData::isNumericDigit(0x31));
	ASSERT_EQUAL(UnicodeData::toNumericValue(0x31), 1);
	
	// Latin 9
	ASSERT_TRUE(UnicodeData::isNumericDigit(0x39));
	ASSERT_EQUAL(UnicodeData::toNumericValue(0x39), 9);

	// Arabic-Indic 0
	ASSERT_TRUE(UnicodeData::isNumericDigit(0x0660))
	ASSERT_EQUAL(UnicodeData::toNumericValue(0x0660), 0);
	
	// Arabic-Indic 9
	ASSERT_TRUE(UnicodeData::isNumericDigit(0x0669))
	ASSERT_EQUAL(UnicodeData::toNumericValue(0x0669), 9);
	
	// Lowercase a is not a digit
	ASSERT_FALSE(UnicodeData::isNumericDigit(0x61));
	ASSERT_EQUAL(UnicodeData::toNumericValue(0x61), UnicodeData::InvalidNumericValue);
}

void testUnicodePredicates()
{
	// Uppercase A (ASCII Lu)
	ASSERT_TRUE(UnicodeData::isUppercase(0x41));
	ASSERT_FALSE(UnicodeData::isLowercase(0x41));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x41));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x41));
	
	// Lowercase A (ASCII Ll)
	ASSERT_FALSE(UnicodeData::isUppercase(0x61));
	ASSERT_TRUE(UnicodeData::isLowercase(0x61));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x61));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x61));
	
	// Latin digit 1 (ASCII Nd)
	ASSERT_FALSE(UnicodeData::isUppercase(0x31));
	ASSERT_FALSE(UnicodeData::isLowercase(0x31));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x31));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x31));
	
	// Space (ASCII Zs)
	ASSERT_FALSE(UnicodeData::isUppercase(0x20));
	ASSERT_FALSE(UnicodeData::isLowercase(0x20));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x20));
	ASSERT_TRUE(UnicodeData::isWhitespace(0x20));
	
	// Uppercase alpha (non-ASCII Lu)
	ASSERT_TRUE(UnicodeData::isUppercase(0x391));
	ASSERT_FALSE(UnicodeData::isLowercase(0x391));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x391));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x391));
	
	// Lowercase alpha (non-ASCII Ll)
	ASSERT_FALSE(UnicodeData::isUppercase(0x3b1));
	ASSERT_TRUE(UnicodeData::isLowercase(0x3b1));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x3b1));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x3b1));
	
	// Arabic-Indic 9 (non-ASCII Nd)
	ASSERT_FALSE(UnicodeData::isUppercase(0x669));
	ASSERT_FALSE(UnicodeData::isLowercase(0x669));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x669));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x669));
	
	// Punctuation space (non-ASCII Zs)
	ASSERT_FALSE(UnicodeData::isUppercase(0x2008));
	ASSERT_FALSE(UnicodeData::isLowercase(0x2008));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x2008));
	ASSERT_TRUE(UnicodeData::isWhitespace(0x2008));
	
	// Roman numeral M (Other_Uppercase)
	ASSERT_TRUE(UnicodeData::isUppercase(0x216F));
	ASSERT_FALSE(UnicodeData::isLowercase(0x216F));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x216F));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x216F));
	
	// Feminine ordinal indicator (Other_Lowercase)
	ASSERT_FALSE(UnicodeData::isUppercase(0xAA));
	ASSERT_TRUE(UnicodeData::isLowercase(0xAA));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0xAA));
	ASSERT_FALSE(UnicodeData::isWhitespace(0xAA));
	
	// Paragraph separator (non-ASCII Zp)
	ASSERT_FALSE(UnicodeData::isUppercase(0x2029));
	ASSERT_FALSE(UnicodeData::isLowercase(0x2029));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x2029));
	ASSERT_TRUE(UnicodeData::isWhitespace(0x2029));
	
	// Dragon (non-ASCII So)
	ASSERT_FALSE(UnicodeData::isUppercase(0x1F409));
	ASSERT_FALSE(UnicodeData::isLowercase(0x1F409));
	ASSERT_FALSE(UnicodeData::isAlphabetic(0x1F409));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x1F409));

	// Han 'castrate; eunuch' (Han Lo)
	ASSERT_FALSE(UnicodeData::isUppercase(0x9609));
	ASSERT_FALSE(UnicodeData::isLowercase(0x9609));
	ASSERT_TRUE(UnicodeData::isAlphabetic(0x9609));
	ASSERT_FALSE(UnicodeData::isWhitespace(0x9609));
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testToUpper();
	testToLower();
	testToFolded();
	testToNumericValue();

	testUnicodePredicates();
}
