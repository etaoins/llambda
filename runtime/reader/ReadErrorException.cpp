#include "ReadErrorException.h"

#include <sstream>

namespace lliby
{

std::string ReadErrorException::message() const
{
	std::ostringstream messageStream;
	messageStream << m_errorType << " at offset " << offset();

	return messageStream.str();
}

}
