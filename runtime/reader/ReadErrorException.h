#ifndef _LLIBY_READER_READERROREXCEPTION_H
#define _LLIBY_READER_READERROREXCEPTION_H

#include <string>

namespace lliby
{

/**
 * Thrown by DatumReader::parse() when an incomplete datum is encountered
 */
class ReadErrorException
{
public:
	explicit ReadErrorException(const std::string &message) :
		m_message(message)
	{
	}

	std::string message() const
	{
		return m_message;
	}

private:
	std::string m_message;
};

}

#endif
