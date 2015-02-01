#include "unicode/UnicodeChar.h"
#include "unicode/utf8.h"
#include "unicode/utf8/InvalidByteSequenceException.h"

#include "core/init.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

auto validOneByte =   reinterpret_cast<const std::uint8_t*>(u8"a");
auto validTwoByte =   reinterpret_cast<const std::uint8_t*>(u8"Б");
auto validThreeByte = reinterpret_cast<const std::uint8_t*>(u8"☃");
auto validFourByte =  reinterpret_cast<const std::uint8_t*>(u8"𠜎");

auto forbiddenByte = reinterpret_cast<const std::uint8_t*>("\xFE");

auto overlongTwoByte =   reinterpret_cast<const std::uint8_t*>("\xC0\xAF");
auto overlongThreeByte = reinterpret_cast<const std::uint8_t*>("\xE0\x80\xAF");
auto overlongFourByte =  reinterpret_cast<const std::uint8_t*>("\xF0\x80\x80\xAF");
auto overlongFiveByte =  reinterpret_cast<const std::uint8_t*>("\xF8\x80\x80\x80\xAF");
auto overlongSixByte =   reinterpret_cast<const std::uint8_t*>("\xFC\x80\x80\x80\x80\xAF");

// These are start characters missing continuation characters
auto lonelyTwoByte =   reinterpret_cast<const std::uint8_t*>("\xC0\x20");
auto lonelyThreeByte = reinterpret_cast<const std::uint8_t*>("\xE0\x20\x20");
auto lonelyFourByte =  reinterpret_cast<const std::uint8_t*>("\xF0\x20\x20\x20");

#define ASSERT_INVALID_ENCODING(begin, end, ExceptionName, errorOffset) \
{ \
	bool caughtException = false; \
 \
	try \
	{ \
		utf8::validateData(begin, end); \
	} \
	catch(const ExceptionName &e) \
	{ \
		ASSERT_EQUAL(e.validChars(), 0); \
		ASSERT_EQUAL(e.startOffset(), 0); \
		ASSERT_EQUAL(e.endOffset(), errorOffset); \
		caughtException = true; \
	} \
	ASSERT_TRUE(caughtException); \
}

void testBytesInSequence()
{
	ASSERT_EQUAL(utf8::bytesInSequence(validOneByte[0]), 1);
	ASSERT_EQUAL(utf8::bytesInSequence(validTwoByte[0]), 2);
	ASSERT_EQUAL(utf8::bytesInSequence(validThreeByte[0]), 3);
	ASSERT_EQUAL(utf8::bytesInSequence(validFourByte[0]), 4);
}

void testBytesForChar()
{
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar(U'a')), 1);
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar(U'Б')), 2);
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar(U'☃')), 3);
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar(U'𠜎')), 4);

	// Invalid
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar()), -1);

	// Out of range
	ASSERT_EQUAL(utf8::bytesForChar(UnicodeChar(0x110000)), -1);
}

void testDecodeChar()
{
	// Valid sequences
	auto validOneByteIt = validOneByte;
	ASSERT_TRUE(utf8::decodeChar(&validOneByteIt) == UnicodeChar(U'a'));
	ASSERT_EQUAL(validOneByteIt - validOneByte, 1);

	auto validTwoByteIt = validTwoByte;
	ASSERT_TRUE(utf8::decodeChar(&validTwoByteIt) == UnicodeChar(U'Б'));
	ASSERT_EQUAL(validTwoByteIt - validTwoByte, 2);

	auto validThreeByteIt = validThreeByte;
	ASSERT_TRUE(utf8::decodeChar(&validThreeByteIt) == UnicodeChar(U'☃'));
	ASSERT_EQUAL(validThreeByteIt - validThreeByte, 3);

	auto validFourByteIt = validFourByte;
	ASSERT_TRUE(utf8::decodeChar(&validFourByteIt) == UnicodeChar(U'𠜎'));
	ASSERT_EQUAL(validFourByteIt - validFourByte, 4);
}

void testValidateData()
{
	// Truncated sequences
	ASSERT_INVALID_ENCODING(validTwoByte, validTwoByte + 1, utf8::TruncatedInputException, 0);

	ASSERT_INVALID_ENCODING(validThreeByte, validThreeByte + 1, utf8::TruncatedInputException, 0);
	ASSERT_INVALID_ENCODING(validThreeByte, validThreeByte + 2, utf8::TruncatedInputException, 1);

	ASSERT_INVALID_ENCODING(validFourByte, validFourByte + 1, utf8::TruncatedInputException, 0);
	ASSERT_INVALID_ENCODING(validFourByte, validFourByte + 2, utf8::TruncatedInputException, 1);
	ASSERT_INVALID_ENCODING(validFourByte, validFourByte + 3, utf8::TruncatedInputException, 2);

	// Forbidden sequence
	ASSERT_INVALID_ENCODING(forbiddenByte, forbiddenByte + 1, utf8::InvalidHeaderByteException, 0);

	// Overlong sequences
	ASSERT_INVALID_ENCODING(overlongTwoByte, overlongTwoByte + 2, utf8::OverlongEncodingException, 1);
	ASSERT_INVALID_ENCODING(overlongThreeByte, overlongThreeByte + 3, utf8::OverlongEncodingException, 2);
	ASSERT_INVALID_ENCODING(overlongFourByte, overlongFourByte + 4, utf8::OverlongEncodingException, 3);

	// These are considered invalid header bytes because we don't support the deprecated > 4 byte sequences
	ASSERT_INVALID_ENCODING(overlongFiveByte, overlongFiveByte + 5, utf8::InvalidHeaderByteException, 0);
	ASSERT_INVALID_ENCODING(overlongSixByte, overlongSixByte + 6, utf8::InvalidHeaderByteException, 0);

	// Lonely sequences are start characters followed by non-continuation bytes
	ASSERT_INVALID_ENCODING(lonelyTwoByte, lonelyTwoByte + 2, utf8::MissingContinuationByteException, 0);
	ASSERT_INVALID_ENCODING(lonelyThreeByte, lonelyThreeByte + 3, utf8::MissingContinuationByteException, 0);
	ASSERT_INVALID_ENCODING(lonelyFourByte, lonelyFourByte + 4, utf8::MissingContinuationByteException, 0);
}

void testAll(World &world)
{
	testBytesInSequence();
	testBytesForChar();
	testDecodeChar();
	testValidateData();
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
