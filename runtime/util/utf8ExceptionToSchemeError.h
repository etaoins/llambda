#ifndef _LLIBY_UTIL_UTF8EXCEPTIONTOSCHEMEERROR_H
#define _LLIBY_UTIL_UTF8EXCEPTIONTOSCHEMEERROR_H

namespace lliby
{

class World;
class AnyCell;

namespace utf8
{
	class InvalidByteSequenceException;
}

/**
 * Converts an internal UTF-8 exception to a Scheme error
 *
 * @param  world     World to signal the error in
 * @param  procName  Scheme name of the calling procedure. This is used in the signalled error message.
 * @param  e         Exception to convert
 * @param  obj       Object that was the source of the UTF-8 encoded data
 */
[[noreturn]]
void utf8ExceptionToSchemeError(World &world, const char *procName, const utf8::InvalidByteSequenceException &e, AnyCell *obj = nullptr);

}

#endif
