#include "unicode/UnicodeChar.h"

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
	ASSERT_EQUAL(UnicodeChar(0x61).toUppercase(), UnicodeChar(0x41));
	// Lowercase z => uppercase Z
	ASSERT_EQUAL(UnicodeChar(0x7a).toUppercase(), UnicodeChar(0x5a));
	
	// Greek alpha => uppercase
	ASSERT_EQUAL(UnicodeChar(0x3b1).toUppercase(), UnicodeChar(0x391));
	// Greek omega => uppercase
	ASSERT_EQUAL(UnicodeChar(0x3c9).toUppercase(), UnicodeChar(0x3a9));

	// Cyrillic small a => uppercase
	ASSERT_EQUAL(UnicodeChar(0x430).toUppercase(), UnicodeChar(0x410));
	// Cyrillic small psi => uppercase
	ASSERT_EQUAL(UnicodeChar(0x471).toUppercase(), UnicodeChar(0x470));

	// Number 1 should be left alone
	ASSERT_EQUAL(UnicodeChar(0x31).toUppercase(), UnicodeChar(0x31));
	
	// The snow man should be left alone
	ASSERT_EQUAL(UnicodeChar(0x2603).toUppercase(), UnicodeChar(0x2603));
}

void testToLower()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(UnicodeChar(0x41).toLowercase(), UnicodeChar(0x61));
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(UnicodeChar(0x5a).toLowercase(), UnicodeChar(0x7a));
	
	// Greek alpha => lowercase 
	ASSERT_EQUAL(UnicodeChar(0x391).toLowercase(), UnicodeChar(0x3b1));
	// Greek omega => lowercase
	ASSERT_EQUAL(UnicodeChar(0x3a9).toLowercase(), UnicodeChar(0x3c9));

	// Cyrillic small a => lowercase
	ASSERT_EQUAL(UnicodeChar(0x410).toLowercase(), UnicodeChar(0x430));
	// Cyrillic small psi => lowercase
	ASSERT_EQUAL(UnicodeChar(0x470).toLowercase(), UnicodeChar(0x471));

	// Number 1 should be left alone
	ASSERT_EQUAL(UnicodeChar(0x31).toLowercase(), UnicodeChar(0x31));
	
	// The snow man should be left alone
	ASSERT_EQUAL(UnicodeChar(0x2603).toLowercase(), UnicodeChar(0x2603));
}

void testToFolded()
{
	// Uppercase A => lowercase a
	ASSERT_EQUAL(UnicodeChar(0x41).toCaseFolded(), UnicodeChar(0x61));
	// Uppercase Z => lowercase z
	ASSERT_EQUAL(UnicodeChar(0x5a).toCaseFolded(), UnicodeChar(0x7a));
	
	// Greek alpha => lowercase 
	ASSERT_EQUAL(UnicodeChar(0x391).toCaseFolded(), UnicodeChar(0x3b1));
	// Greek omega => lowercase
	ASSERT_EQUAL(UnicodeChar(0x3a9).toCaseFolded(), UnicodeChar(0x3c9));

	// Small letter long s => lowercase S
	// This is a difference between folding and toLower
	ASSERT_EQUAL(UnicodeChar(0x017F).toCaseFolded(), UnicodeChar(0x73))
}

void testToNumberCell()
{
	// Latin 1
	ASSERT_TRUE(UnicodeChar(0x31).isNumericDigit());
	ASSERT_EQUAL(UnicodeChar(0x31).digitValue(), 1);
	
	// Latin 9
	ASSERT_TRUE(UnicodeChar(0x39).isNumericDigit());
	ASSERT_EQUAL(UnicodeChar(0x39).digitValue(), 9);

	// Arabic-Indic 0
	ASSERT_TRUE(UnicodeChar(0x0660).isNumericDigit())
	ASSERT_EQUAL(UnicodeChar(0x0660).digitValue(), 0);
	
	// Arabic-Indic 9
	ASSERT_TRUE(UnicodeChar(0x0669).isNumericDigit())
	ASSERT_EQUAL(UnicodeChar(0x0669).digitValue(), 9);
	
	// Lowercase a is not a digit
	ASSERT_FALSE(UnicodeChar(0x61).isNumericDigit());
	ASSERT_EQUAL(UnicodeChar(0x61).digitValue(), UnicodeChar::InvalidDigitValue);
}

void testUnicodePredicates()
{
	{
		// Uppercase A (ASCII Lu)
		const UnicodeChar uppercaseA(0x41);

		ASSERT_TRUE(uppercaseA.isUppercase());
		ASSERT_FALSE(uppercaseA.isLowercase());
		ASSERT_TRUE(uppercaseA.isAlphabetic());
		ASSERT_FALSE(uppercaseA.isWhitespace());
	}
	
	{
		// Lowercase A (ASCII Ll)
		const UnicodeChar lowercaseA(0x61);
		
		ASSERT_FALSE(lowercaseA.isUppercase());
		ASSERT_TRUE(lowercaseA.isLowercase());
		ASSERT_TRUE(lowercaseA.isAlphabetic());
		ASSERT_FALSE(lowercaseA.isWhitespace());
	}
	
	{
		// Latin digit 1 (ASCII Nd)
		const UnicodeChar latinDigit1(0x31);

		ASSERT_FALSE(latinDigit1.isUppercase());
		ASSERT_FALSE(latinDigit1.isLowercase());
		ASSERT_FALSE(latinDigit1.isAlphabetic());
		ASSERT_FALSE(latinDigit1.isWhitespace());
	}
	
	{
		// Space (ASCII Zs)
		const UnicodeChar space(0x20);

		ASSERT_FALSE(space.isUppercase());
		ASSERT_FALSE(space.isLowercase());
		ASSERT_FALSE(space.isAlphabetic());
		ASSERT_TRUE(space.isWhitespace());
	}
	
	{
		// Uppercase alpha (non-ASCII Lu)
		const UnicodeChar uppercaseAlpha(0x391);

		ASSERT_TRUE(uppercaseAlpha.isUppercase());
		ASSERT_FALSE(uppercaseAlpha.isLowercase());
		ASSERT_TRUE(uppercaseAlpha.isAlphabetic());
		ASSERT_FALSE(uppercaseAlpha.isWhitespace());
	}
	
	{
		// Lowercase alpha (non-ASCII Ll)
		const UnicodeChar lowercaseAlpha(0x3b1);

		ASSERT_FALSE(lowercaseAlpha.isUppercase());
		ASSERT_TRUE(lowercaseAlpha.isLowercase());
		ASSERT_TRUE(lowercaseAlpha.isAlphabetic());
		ASSERT_FALSE(lowercaseAlpha.isWhitespace());
	}
	
	{
		// Arabic-Indic 9 (non-ASCII Nd)
		const UnicodeChar arabicIndic9(0x669);

		ASSERT_FALSE(arabicIndic9.isUppercase());
		ASSERT_FALSE(arabicIndic9.isLowercase());
		ASSERT_FALSE(arabicIndic9.isAlphabetic());
		ASSERT_FALSE(arabicIndic9.isWhitespace());
	}
	
	{
		// Punctuation space (non-ASCII Zs)
		const UnicodeChar punctuationSpace(0x2008);

		ASSERT_FALSE(punctuationSpace.isUppercase());
		ASSERT_FALSE(punctuationSpace.isLowercase());
		ASSERT_FALSE(punctuationSpace.isAlphabetic());
		ASSERT_TRUE(punctuationSpace.isWhitespace());
	}
	
	{
		// Roman numeral M (Other_Uppercase)
		const UnicodeChar romanNumeralM(0x216F);

		ASSERT_TRUE(romanNumeralM.isUppercase());
		ASSERT_FALSE(romanNumeralM.isLowercase());
		ASSERT_TRUE(romanNumeralM.isAlphabetic());
		ASSERT_FALSE(romanNumeralM.isWhitespace());
	}
	
	{
		// Feminine ordinal indicator (Other_Lowercase)
		const UnicodeChar feminineOrdinalIndicator(0xAA);

		ASSERT_FALSE(feminineOrdinalIndicator.isUppercase());
		ASSERT_TRUE(feminineOrdinalIndicator.isLowercase());
		ASSERT_TRUE(feminineOrdinalIndicator.isAlphabetic());
		ASSERT_FALSE(feminineOrdinalIndicator.isWhitespace());
	}
	
	{
		// Paragraph separator (non-ASCII Zp)
		const UnicodeChar paragraphSeparator(0x2029);

		ASSERT_FALSE(paragraphSeparator.isUppercase());
		ASSERT_FALSE(paragraphSeparator.isLowercase());
		ASSERT_FALSE(paragraphSeparator.isAlphabetic());
		ASSERT_TRUE(paragraphSeparator.isWhitespace());
	}
	
	{
		// Dragon (non-ASCII So)
		const UnicodeChar dragon(0x1F409);

		ASSERT_FALSE(dragon.isUppercase());
		ASSERT_FALSE(dragon.isLowercase());
		ASSERT_FALSE(dragon.isAlphabetic());
		ASSERT_FALSE(dragon.isWhitespace());
	}

	{
		// Han 'castrate; eunuch' (Han Lo)
		const UnicodeChar castrateEunuch(0x9609);

		ASSERT_FALSE(castrateEunuch.isUppercase());
		ASSERT_FALSE(castrateEunuch.isLowercase());
		ASSERT_TRUE(castrateEunuch.isAlphabetic());
		ASSERT_FALSE(castrateEunuch.isWhitespace());
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
	ASSERT_TRUE(uppercaseA.compare(smallA, CaseSensitivity::Insensitive) == 0);
	
	ASSERT_TRUE(smallAlpha.compare(capitalAlpha) > 0);
	ASSERT_TRUE(capitalAlpha.compare(smallAlpha) < 0);
	ASSERT_TRUE(capitalAlpha.compare(smallAlpha, CaseSensitivity::Insensitive) == 0);
	
	ASSERT_TRUE(dragon.compare(snowman) > 0);
	ASSERT_TRUE(snowman.compare(dragon) < 0);
	ASSERT_FALSE(snowman.compare(dragon, CaseSensitivity::Insensitive) == 0);
	
	ASSERT_TRUE(capitalAlpha.compare(smallA) > 0);
	ASSERT_TRUE(smallA.compare(capitalAlpha) < 0);
	ASSERT_FALSE(smallA.compare(capitalAlpha, CaseSensitivity::Insensitive) == 0);

	ASSERT_TRUE(smallS.compare(smallLongS, CaseSensitivity::Insensitive) == 0);
	ASSERT_FALSE(smallS.compare(smallLongS) == 0);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	testToUpper();
	testToLower();
	testToFolded();
	testToNumberCell();

	testUnicodePredicates();

	testCompare();
}
