#ifndef _LLIBY_CORE_FATAL_H
#define _LLIBY_CORE_FATAL_H

namespace lliby
{

class DatumCell;

}

extern "C"
{

[[noreturn]]
void _lliby_fatal(const char *message, const lliby::DatumCell *evidence = nullptr);

}

#endif
