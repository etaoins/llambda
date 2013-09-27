#ifndef _LLIBY_BINDING_BOXEDSTRING_H
#define _LLIBY_BINDING_BOXEDSTRING_H

#include "BoxedStringLike.h"
#include <list>

#include "unicode/UnicodeChar.h"

namespace lliby
{

class BoxedString : public BoxedStringLike
{
#include "generated/BoxedStringMembers.h"
public:
	BoxedString(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		BoxedStringLike(BoxedTypeId::String, utf8Data, byteLength, charLength)
	{
	}

	static BoxedString* fromUtf8CString(const char *str);
	static BoxedString* fromUtf8Data(const std::uint8_t *data, std::uint32_t byteLength);
	static BoxedString* fromFill(std::uint32_t length, UnicodeChar fill);
	static BoxedString* fromAppended(const std::list<const BoxedString*> &strings);
	static BoxedString* fromUnicodeChars(const std::list<UnicodeChar> &unicodeChars);

	BoxedString* copy(std::int64_t start = 0, std::int64_t end = -1); 

	UnicodeChar charAt(std::uint32_t offset) const;
	bool setCharAt(std::uint32_t offset, UnicodeChar unicodeChar);

	bool fill(UnicodeChar unicodeChar, std::int64_t start = 0, std::int64_t end = -1);
	bool replace(std::uint32_t offset, const BoxedString *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	std::list<UnicodeChar> unicodeChars(std::int64_t start = 0, std::int64_t end = -1) const;

	bool operator==(const BoxedString &other) const
	{
		return equals(other);
	}
	
	bool operator!=(const BoxedString &other) const
	{
		return !equals(other);
	}

	// Returns and integer less than, equal to or greater than zero if the string
	// less than, equal to or greater than the other string
	int compare(const BoxedString *other, CaseSensitivity cs = CaseSensitivity::Sensitive) const;

	bool asciiOnly() const
	{
		return byteLength() == charLength();
	}

	BoxedSymbol *toSymbol() const;
	BoxedByteVector *toUtf8ByteVector(std::int64_t start = 0, std::int64_t end = -1) const;
	
	BoxedString *toUppercaseString() const;
	BoxedString *toLowercaseString() const;
	BoxedString *toCaseFoldedString() const;

private:
	std::uint8_t *charPointer(std::uint8_t *scanFrom, std::uint32_t bytesLeft, uint32_t charOffset) const;
	std::uint8_t *charPointer(std::uint32_t charOffset) const;

	struct CharRange
	{
		std::uint8_t *startPointer;
		std::uint8_t *endPointer;
		std::uint32_t charCount;

		bool isNull() const
		{
			return startPointer == nullptr;
		};

		unsigned int byteCount() const
		{
			return endPointer - startPointer;
		}
	};

	CharRange charRange(std::int64_t start, std::int64_t end = -1) const; 
	bool replaceBytes(const CharRange &range, std::uint8_t *pattern, unsigned int patternBytes, unsigned int count = 1);
	
	int compareCaseSensitive(const BoxedString *other) const;
	int compareCaseInsensitive(const BoxedString *other) const;

	BoxedString *toConvertedString(UnicodeChar (UnicodeChar::* converter)() const) const;
};

}

#endif

