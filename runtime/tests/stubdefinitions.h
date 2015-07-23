#ifndef _LLIBY_TEST_STUBDEFINITIONS
#define _LLIBY_TEST_STUBDEFINITIONS

#include "classmap/RecordClassMap.h"

extern "C"
{
	RecordClassMap EmptyRecordClassMap {.totalSize = 0, .offsetCount = 0};

	// This is normally supplied by the compiler
	const RecordClassMap *_llambda_compiler_class_map[1] {&EmptyRecordClassMap};
}

#endif
