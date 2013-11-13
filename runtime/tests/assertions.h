#ifndef _LLIBY_TEST_ASSERTIONS_H
#define _LLIBY_TEST_ASSERTIONS_H

#include <stdlib.h>
#include <string.h>
#include <iostream>

namespace
{

#define ASSERT_EQUAL(actual, expected) \
{ \
	if (actual != expected) \
	{ \
		std::cerr << "\"" << actual << "\" does not match expected value \"" << expected << "\"" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_UTF8_EQUAL(actual, expected) \
{ \
	if (strcmp(reinterpret_cast<const char *>(actual), reinterpret_cast<const char *>(expected)) != 0) \
	{ \
		std::cerr << "\"" << reinterpret_cast<const char *>(actual) \
		          << "\" does not match expected value \"" << reinterpret_cast<const char *>(expected) << "\"" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_TRUE(value) \
{ \
	if (value != true) \
	{ \
		std::cerr << "Value unexpectedly false" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_FALSE(value) \
{ \
	if (value != false) \
	{ \
		std::cerr << "Value unexpectedly true" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_NULL(value) \
{ \
	if (value != nullptr) \
	{ \
		std::cerr << "Value unexpectedly non-null" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

}

#endif

