#ifndef _LLIBY_CORE_ERROR_H
#define _LLIBY_CORE_ERROR_H

#include <vector>
#include "binding/AnyCell.h"
#include "binding/ErrorCategory.h"

namespace lliby
{

class World;

[[noreturn]]
void signalError(World &world, const char *message, const std::vector<AnyCell*> &irritants = std::vector<AnyCell*>(), ErrorCategory category = ErrorCategory::Default);

[[noreturn]]
void fatalError(const char *message, const lliby::AnyCell *evidence = nullptr);

}

#endif
