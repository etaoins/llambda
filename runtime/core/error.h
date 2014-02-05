#ifndef _LLIBY_CORE_ERROR_H
#define _LLIBY_CORE_ERROR_H

#include <vector>
#include "binding/DatumCell.h"

namespace lliby
{

[[noreturn]]
void signalError(const char *message, const std::vector<DatumCell*> &irritants = std::vector<DatumCell*>());

[[noreturn]]
void fatalError(const char *message, const lliby::DatumCell *evidence = nullptr);

}

#endif
