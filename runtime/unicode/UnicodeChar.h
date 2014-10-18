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
	explicit UnicodeChar(std::int32_t codePoint = InvalidCodePoint) :
		m_codePoint(codePoint)
	{
	}

	bool isValid() const
	{
		return m_codePoint != InvalidCodePoint;
	}

	std::int32_t codePoint() const
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
	static const std::int32_t InvalidCodePoint = -1;

	std::int32_t m_codePoint;
};

}

#endif
