# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(lliby
	alloc/CellRefRangeList.cpp
	alloc/DynamicMemoryBlock.cpp
	alloc/Finalizer.cpp
	alloc/Heap.cpp
	alloc/allocator.cpp
	alloc/cellvisitor.cpp
	alloc/collector.cpp
	binding/BytevectorCell.cpp
	binding/CharacterCell.cpp
	binding/DatumCell.cpp
	binding/ErrorObjectCell.cpp
	binding/ListElementCell.cpp
	binding/PairCell.cpp
	binding/ProcedureCell.cpp
	binding/RecordCell.cpp
	binding/RecordLikeCell.cpp
	binding/StringCell.cpp
	binding/SymbolCell.cpp
	binding/VectorCell.cpp
	core/World.cpp
	core/constinstances.cpp
	core/dynamic.cpp
	core/error.cpp
	core/init.cpp
	core/recorddata.cpp
	dynamic/State.cpp
	dynamic/EscapeProcedureCell.cpp
	dynamic/ParameterProcedureCell.cpp
	dynamic/init.cpp
	platform/memory.cpp
	stdlib/generated/predicates.cpp
	stdlib/boolean.cpp
	stdlib/bytevector.cpp
	stdlib/character.cpp
	stdlib/control.cpp
	stdlib/dynamic.cpp
	stdlib/eqv.cpp
	stdlib/exception.cpp
	stdlib/list.cpp
	stdlib/number.cpp
	stdlib/output.cpp
	stdlib/process.cpp
	stdlib/string.cpp
	stdlib/symbol.cpp
	stdlib/vector.cpp
	unicode/UnicodeChar.cpp
	writer/ExternalFormDatumWriter.cpp
	)

include_directories(${lliby_SOURCE_DIR})

# FreeBSD and Linux need special threading flags
find_package (Threads)

# Create compile_commands.json. Clang tools like this
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# Enable optimizations by default
if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

# Be verbose with warnings
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

# In debug builds additionally error out on warnings
# This isn't enabled for release as different compiler versions and configurations can cause unexpected warnings
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Werror")

# Enable C++11
ADD_DEFINITIONS(-std=c++11)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	# Mac OS X < 10.9 used an old version of libstdc++ as the default C++ stdlib. It did not support C++11.
	# Explicitly use libc++ to enable full C++11 support
	# This workaround and the matching one in codegen can be removed once Mac OS X 10.8 is unsupported.
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

# Force colour output so Clang++ prints pretty errors in Ninja
if (${CMAKE_GENERATOR} STREQUAL "Ninja")
	ADD_DEFINITIONS(-fcolor-diagnostics)
endif()

# GC debugging mode
set(ENABLE_GC_DEBUGGING "no" CACHE STRING "Enable extra garbage collector sanity checks at a severe performance penalty")
if (${ENABLE_GC_DEBUGGING} STREQUAL "yes")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_LLIBY_ALWAYS_GC -D_LLIBY_NO_ADDR_REUSE -D_LLIBY_CHECK_LEAKS")
endif()

# Add tests
include(CTest)
set(CTEST_MEMCHECK_COMMAND "valgrind")

set(ALL_TEST_NAMES
	allocator
	bytevector
	constinstances
	implicitsharing
	inexactrational
	listelement
	sharedbytearray
	string
	unicodechar
	vector
	externalformdatumwriter
	properlist)

foreach( test_name ${ALL_TEST_NAMES} )
	add_executable(tests/bin/test-${test_name} tests/test-${test_name}.cpp)
	target_link_libraries(tests/bin/test-${test_name} lliby ${CMAKE_THREAD_LIBS_INIT})

	add_test(${test_name} tests/bin/test-${test_name})
endforeach()
