#ifndef _LLIBY_UNICODE_UTF8_INVALIDBYTESEQUENCEEXCEPTION_H
#define _LLIBY_UNICODE_UTF8_INVALIDBYTESEQUENCEEXCEPTION_H

#include <cstdint>
#include <string>

namespace lliby
{
namespace utf8
{

/**
 * Thrown by utf8::validateData when an invalid byte sequence is encountered
 */
class InvalidByteSequenceException
{
public:
	/**
	 * The number of valid characters encountered before the invalid byte sequence
	 */
	std::size_t validChars() const
	{
		return m_validChars;
	}

	/**
	 * The offset in bytes of the start of the invalid byte sequence
	 */
	std::size_t startOffset() const
	{
		return m_startOffset;
	}

	/**
	 * The offset in bytes of the end of the invalid byte sequence
	 *
	 * This is useful for error reporting or restarting UTF-8 parsing after the invalid sequence
	 */
	std::size_t endOffset() const
	{
		return m_endOffset;
	}

	/**
	 * Returns a user-readable string describing the invalid byte sequence
	 */
	std::string message() const;

protected:
	InvalidByteSequenceException(std::size_t validChars, std::size_t startOffset, std::size_t endOffset, const char *errorType) :
		m_validChars(validChars),
		m_startOffset(startOffset),
		m_endOffset(endOffset),
		m_errorType(errorType)
	{
	}

private:
	std::size_t m_validChars;
	std::size_t m_startOffset;
	std::size_t m_endOffset;
	const char *m_errorType;
};

class InvalidHeaderByteException : public InvalidByteSequenceException
{
public:
	InvalidHeaderByteException(std::size_t validChars, std::size_t offset) :
		InvalidByteSequenceException(validChars, offset, offset, "Invalid header byte")
	{
	}
};

class TruncatedInputException : public InvalidByteSequenceException
{
public:
	TruncatedInputException(std::size_t validChars, std::size_t startOffset, std::size_t endOffset) :
		InvalidByteSequenceException(validChars, startOffset, endOffset, "Truncated input")
	{
	}
};

class MissingContinuationByteException : public InvalidByteSequenceException
{
public:
	MissingContinuationByteException(std::size_t validChars, std::size_t startOffset, std::size_t endOffset) :
		InvalidByteSequenceException(validChars, startOffset, endOffset, "Missing continuation byte")
	{
	}
};

class OverlongEncodingException : public InvalidByteSequenceException
{
public:
	OverlongEncodingException(std::size_t validChars, std::size_t startOffset, std::size_t endOffset) :
		InvalidByteSequenceException(validChars, startOffset, endOffset, "Overlong encoding")
	{
	}
};

}
}

#endif
