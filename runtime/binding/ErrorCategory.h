#ifndef _LLIBY_BINDING_ERRORCATEGORY_H
#define _LLIBY_BINDING_ERRORCATEGORY_H

#include <cstdint>

namespace lliby
{

/**
 * High-level categorisation for Scheme errors
 *
 * The category of an error object determines which error category predicates it satisfies in Scheme. For instance, only
 * errors with the category of ErrorCategory::File satisfy the (file-error?) predicate.
 *
 * Only ErrorCategory::File and ErrorCategory::Read are required by R7RS. The other values are defined by Llambda
 * loosely based on the exception model of Racket.
 */
enum class ErrorCategory : std::uint16_t
{
	/**
	 * Unclassified error
	 */
	Default = 0,

	/**
	 * Error related to filesystem operations
	 *
	 * These satisfy the (file-error?) predicate from R7RS. Use caution to only signal these errors in a manner
	 * consistent with R7RS
	 */
	File = 1,

	/**
	 * Error due to invalid input to the (read) procedure
	 *
	 * These satisfy the (read-error?) predicate for R7RS. Use caution to only signal these errors in a manner
	 * consistent with R7RS
	 */
	Read = 2,

	/**
	 * Error due to a type system violation
	 *
	 * This is typically used for impossible conversions between types
	 */
	Type = 3,

	/**
	 * Error due to applying a procedure with an incompatible number of operands
	 *
	 * This is also signalled when attempting unpack multiple value return with a mismatched number of values.
	 */
	Arity = 4,

	/**
	 * Error due to an out-of-range value
	 *
	 * This is typically used for indices in to collections or integer arguments with invalid values
	 */
	Range = 5,

	/**
	 * Error due to invalid UTF-8 encoding
	 *
	 * @sa utf8::InvalidByteSequenceException
	 * @sa utf8ExceptionToSchemeError
	 */
	Utf8 = 6,

	/**
	 * Error due to attempted division by zero
	 */
	DivideByZero = 7,

	/**
	 * Error due to attempt mutation of a literal object
	 */
	MutateLiteral = 8,

	/**
	 * Error due to accessing a variable before its definition
	 */
	UndefinedVariable = 9
};

}

#endif
