#ifndef _LLIBY_TEST_ASSERTIONS_H
#define _LLIBY_TEST_ASSERTIONS_H

#include <stdlib.h>
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

}

#endif

