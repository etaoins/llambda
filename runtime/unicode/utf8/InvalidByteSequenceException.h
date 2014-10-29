#ifndef _LLIBY_UNICODE_UTF8_INVALIDBYTESEQUENCEEXCEPTION_H
#define _LLIBY_UNICODE_UTF8_INVALIDBYTESEQUENCEEXCEPTION_H

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
	explicit InvalidByteSequenceException(size_t byteOffset) :
		m_byteOffset(byteOffset)
	{
	}

	/**
	 * The offset in bytes of the end of the invalid byte sequence
	 *
	 * This is useful for error reporting or restarting UTF-8 parsing after the invalid sequence
	 */
	size_t byteOffset() const
	{
		return m_byteOffset;
	}

private:
	size_t m_byteOffset;
};


}
}

#endif
