#include "InvalidByteSequenceException.h"

#include <sstream>

namespace lliby
{
namespace utf8
{

std::string InvalidByteSequenceException::message() const
{
	std::ostringstream messageStream;

	if (startOffset() == endOffset())
	{
		messageStream << m_errorType << " at byte offset " << startOffset();
	}
	else
	{
		messageStream << m_errorType << " at byte range " << startOffset() << "-" << endOffset();
	}

	return messageStream.str();
}

}
}
