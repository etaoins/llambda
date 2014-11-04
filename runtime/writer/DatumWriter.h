#ifndef _LLIBY_WRITER_DATUMWRITER_H
#define _LLIBY_WRITER_DATUMWRITER_H

#include "binding/generated/declaretypes.h"

namespace lliby
{

class DatumWriter
{
public:
	virtual void render(const AnyCell *datum, int defaultRadix = 10) = 0;
};

}

#endif

