#include "utf8ExceptionToSchemeError.h"

#include <sstream>

#include "unicode/utf8/InvalidByteSequenceException.h"
#include "core/error.h"

namespace lliby
{

void utf8ExceptionToSchemeError(World &world, const char *procName, const utf8::InvalidByteSequenceException &e, AnyCell *obj)
{
	std::ostringstream message;
	message << "Invalid UTF-8 in " << procName << ": " << e.message();

	if (obj != nullptr)
	{
		signalError(world, message.str().c_str(), {obj});
	}
	else
	{
		signalError(world, message.str().c_str());
	}
}

}
