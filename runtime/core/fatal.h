#ifndef _CORE_FATAL_H
#define _CORE_FATAL_H

namespace lliby
{

class BoxedDatum;

}

extern "C"
{

void _lliby_fatal(const char *message, const lliby::BoxedDatum *evidence = nullptr);

}

#endif
