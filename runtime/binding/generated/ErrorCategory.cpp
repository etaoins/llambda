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
	case ErrorCategory::IntegerOverflow:
		return "integer-overflow-error";
	case ErrorCategory::ImplementationRestriction:
		return "implementation-restriction-error";
	case ErrorCategory::UnclonableValue:
		return "unclonable-value-error";
	case ErrorCategory::NoActor:
		return "no-actor-error";
	case ErrorCategory::ExpiredEscapeProcedure:
		return "expired-escape-procedure-error";
	case ErrorCategory::AskTimeout:
		return "ask-timeout-error";
	case ErrorCategory::Match:
		return "match-error";
	}
}

}
