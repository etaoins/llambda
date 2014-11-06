#ifndef _LLIBY_READER_READERROREXCEPTION_H
#define _LLIBY_READER_READERROREXCEPTION_H

#include <string>
#include <cstdint>

namespace lliby
{

/**
 * Thrown by DatumReader::parse() when an incomplete datum is encountered
 */
class ReadErrorException
{
public:
	ReadErrorException(std::size_t offset, const char *errorType) :
		m_offset(offset),
		m_errorType(errorType)
	{
	}

	std::size_t offset() const
	{
		return m_offset;
	}

	std::string message() const;

private:
	std::size_t m_offset;
	const char *m_errorType;
};

}

#endif
