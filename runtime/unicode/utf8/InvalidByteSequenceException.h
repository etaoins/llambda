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
	explicit InvalidByteSequenceException(std::size_t startOffset, std::size_t endOffset, const char *errorType) :
		m_startOffset(startOffset),
		m_endOffset(endOffset),
		m_errorType(errorType)
	{
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

	/**
	 * Returns a copy of this exception offset by the given number of bytes
	 */
	InvalidByteSequenceException offsetBy(size_t byteOffset) const;

private:
	std::size_t m_startOffset;
	std::size_t m_endOffset;
	const char *m_errorType;
};


}
}

#endif
