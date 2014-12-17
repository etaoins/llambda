#ifndef _LLIBY_CORE_ERROR_H
#define _LLIBY_CORE_ERROR_H

#include <vector>
#include "binding/AnyCell.h"
#include "binding/ErrorCategory.h"

namespace lliby
{

class World;

[[noreturn]]
void signalError(World &world, ErrorCategory category, const char *message, const std::vector<AnyCell*> &irritants = std::vector<AnyCell*>());

[[noreturn]]
void fatalError(const char *message, const lliby::AnyCell *evidence = nullptr);


/**
 * Variant of cell_cast that raises an error if the cast fails
 */
template <class T>
T* cell_checked_cast(World &world, AnyCell *cellValue, const char *message)
{
	T *result = cell_cast<T>(cellValue);

	if (result == nullptr)
	{
		signalError(world, ErrorCategory::Type, message, {cellValue});
	}

	return result;
}

template <class T>
const T* cell_checked_cast(World &world, const AnyCell *cellValue, const char *message)
{
	T *result = cell_cast<T>(cellValue);

	if (result == nullptr)
	{
		signalError(world, ErrorCategory::Type, message, {const_cast<AnyCell*>(cellValue)});
	}

	return result;
}

}

#endif
