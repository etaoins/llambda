#ifndef _LLIBY_CORE_ERROR_H
#define _LLIBY_CORE_ERROR_H

#include <vector>
#include "binding/AnyCell.h"

namespace lliby
{

class World;

[[noreturn]]
void signalError(World &world, const char *message, const std::vector<AnyCell*> &irritants = std::vector<AnyCell*>(), const char *path = nullptr, unsigned int lineNumber = 0);

[[noreturn]]
void fatalError(const char *message, const lliby::AnyCell *evidence = nullptr);

}

#endif
