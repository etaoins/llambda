#ifndef _LLIBY_UNICODE_UNICODECHAR_H
#define _LLIBY_UNICODE_UNICODECHAR_H

#include <cstdint>

namespace lliby
{

enum class CaseSensitivity
{
	Sensitive,
	Insensitive
};

class UnicodeChar
{
public:
	using CodePoint = std::int32_t;

	/**
	 * First valid Unicode code point
	 *
	 * This is Unicode NULL
	 */
	static const CodePoint FirstCodePoint = 0x0;

	/**
	 * Last representable Unicode code point
	 *
	 * This is the maximum value that can be represented in UTF-16. No code points will be assigned past this value for
	 * compatibility.
	 */
	static const CodePoint LastCodePoint = 0x10FFFF;

	explicit UnicodeChar(CodePoint codePoint = InvalidCodePoint) :
		m_codePoint(codePoint)
	{
	}

	/**
	 * Returns if this character contains a valid Unicode code point
	 */
	bool isValid() const
	{
		return (m_codePoint >= FirstCodePoint) && (m_codePoint < LastCodePoint);
	}

	CodePoint codePoint() const
	{
		return m_codePoint;
	}
	
	bool isUppercase() const;
	bool isLowercase() const;
	bool isAlphabetic() const;
	bool isWhitespace() const;
	bool isNumericDigit() const;
	
	UnicodeChar toUppercase() const;
	UnicodeChar toLowercase() const;
	UnicodeChar toCaseFolded() const;
	
	typedef std::int32_t DigitValue;
	static const DigitValue InvalidDigitValue = -1;

	DigitValue digitValue() const;

	bool operator==(const UnicodeChar &other) const
	{
		return codePoint() == other.codePoint();
	}
	
	bool operator!=(const UnicodeChar &other) const
	{
		return codePoint() != other.codePoint();
	}

	bool operator<(const UnicodeChar &other) const
	{
		return codePoint() < other.codePoint();
	}

	bool operator<=(const UnicodeChar &other) const
	{
		return codePoint() <= other.codePoint();
	}

	bool operator>(const UnicodeChar &other) const
	{
		return codePoint() > other.codePoint();
	}

	bool operator>=(const UnicodeChar &other) const
	{
		return codePoint() >= other.codePoint();
	}

	int compare(const UnicodeChar &other, CaseSensitivity cs = CaseSensitivity::Sensitive) const
	{
		switch(cs)
		{
		case CaseSensitivity::Sensitive:
			return codePoint() - other.codePoint();
		case CaseSensitivity::Insensitive:
			return toCaseFolded().codePoint() - other.toCaseFolded().codePoint();
		}
	}

private:
	static const CodePoint InvalidCodePoint = -1;

	CodePoint m_codePoint;
};

}

#endif
