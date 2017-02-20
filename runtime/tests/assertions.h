#ifndef _LLIBY_TEST_ASSERTIONS_H
#define _LLIBY_TEST_ASSERTIONS_H

#include <cstdlib>
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

#define ASSERT_UTF8_EQUAL(actualStringCell, expected) \
{ \
	auto length = sizeof(expected) - 1;\
	ASSERT_EQUAL(actualStringCell->byteLength(), length); \
	if (memcmp(reinterpret_cast<const char *>(actualStringCell->constUtf8Data()), reinterpret_cast<const char *>(expected), length) != 0) \
	{ \
		std::cerr << "\"" \
		          << actualStringCell \
		          << "\" does not match expected value \"" << reinterpret_cast<const char *>(expected) << "\"" \
			       << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_TRUE(value) \
{ \
	if ((value) != true) \
	{ \
		std::cerr << "Value unexpectedly false" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_FALSE(value) \
{ \
	if ((value) != false) \
	{ \
		std::cerr << "Value unexpectedly true" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

#define ASSERT_NULL(value) \
{ \
	if ((value) != nullptr) \
	{ \
		std::cerr << "Value unexpectedly non-null" \
			      << " at line " << __LINE__ << std::endl; \
		exit(-1); \
	} \
}

}

#endif

