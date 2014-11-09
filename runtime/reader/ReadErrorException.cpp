#include "ReadErrorException.h"

#include <sstream>

namespace lliby
{

std::string ReadErrorException::message() const
{
	if (offset() != UnknownOffset)
	{
		std::ostringstream messageStream;
		messageStream << m_errorType << " at offset " << offset();

		return messageStream.str();
	}
	else
	{
		return m_errorType;
	}
}

}
