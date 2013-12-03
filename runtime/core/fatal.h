#ifndef _CORE_FATAL_H
#define _CORE_FATAL_H

namespace lliby
{

class DatumCell;

}

extern "C"
{

void _lliby_fatal(const char *message, const lliby::DatumCell *evidence = nullptr);

}

#endif
