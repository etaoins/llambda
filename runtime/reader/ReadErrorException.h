#ifndef _LLIBY_READER_READERROREXCEPTION_H
#define _LLIBY_READER_READERROREXCEPTION_H

#include <string>
#include <cstdint>

namespace lliby
{

/**
 * Thrown by DatumReader::parse() when an invalid or incomplete datum is encountered
 */
class ReadErrorException
{
public:
	static const int UnknownOffset = -1;

	int offset() const
	{
		return m_offset;
	}

	std::string message() const;

protected:
	ReadErrorException(int offset, const char *errorType) :
		m_offset(offset),
		m_errorType(errorType)
	{
	}

private:
	int m_offset;
	const char *m_errorType;
};

class MalformedDatumException : public ReadErrorException
{
public:
	MalformedDatumException(int offset, const char *errorType) :
		ReadErrorException(offset, errorType)
	{
	}
};

class UnexpectedEofException : public ReadErrorException
{
public:
	UnexpectedEofException(int offset, const char *errorType) :
		ReadErrorException(offset, errorType)
	{
	}
};

}

#endif
