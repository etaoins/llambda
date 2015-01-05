#include "ErrorCategory.h"

namespace lliby
{

const char *schemeNameForErrorCategory(ErrorCategory category)
{
	switch(category)
	{
	case ErrorCategory::Default:
		return "default-error";
	case ErrorCategory::File:
		return "file-error";
	case ErrorCategory::Read:
		return "read-error";
	case ErrorCategory::Type:
		return "type-error";
	case ErrorCategory::Arity:
		return "arity-error";
	case ErrorCategory::Range:
		return "range-error";
	case ErrorCategory::Utf8:
		return "utf8-error";
	case ErrorCategory::DivideByZero:
		return "divide-by-zero-error";
	case ErrorCategory::MutateLiteral:
		return "mutate-literal-error";
	case ErrorCategory::UndefinedVariable:
		return "undefined-variable-error";
	case ErrorCategory::OutOfMemory:
		return "out-of-memory-error";
	case ErrorCategory::InvalidArgument:
		return "invalid-argument-error";
	}
}

}
