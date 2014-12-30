// This file is automatically generated by generate-error-categories.scm. Do not edit manually.

#include "binding/AnyCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/ErrorCategory.h"

using namespace lliby;

namespace
{
	bool isErrorObjectOfCategory(AnyCell *obj, ErrorCategory expected)
	{
		if (auto errorObj = cell_cast<ErrorObjectCell>(obj))
		{
			return errorObj->category() == expected;
		}

		return false;
	}
}

extern "C"
{


bool llerror_is_type_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::Type);
}

bool llerror_is_arity_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::Arity);
}

bool llerror_is_range_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::Range);
}

bool llerror_is_utf8_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::Utf8);
}

bool llerror_is_divide_by_zero_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::DivideByZero);
}

bool llerror_is_mutate_literal_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::MutateLiteral);
}

bool llerror_is_undefined_variable_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::UndefinedVariable);
}

bool llerror_is_out_of_memory_error(AnyCell *obj)
{
	return isErrorObjectOfCategory(obj, ErrorCategory::OutOfMemory);
}

}
