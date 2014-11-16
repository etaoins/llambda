# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(lliby
	alloc/CellRootList.cpp
	alloc/DynamicMemoryBlock.cpp
	alloc/Finalizer.cpp
	alloc/Heap.cpp
	alloc/allocator.cpp
	alloc/collector.cpp
	binding/BytevectorCell.cpp
	binding/CharCell.cpp
	binding/AnyCell.cpp
	binding/DynamicStateCell.cpp
	binding/ErrorObjectCell.cpp
	binding/ListElementCell.cpp
	binding/NumberCell.cpp
	binding/PairCell.cpp
	binding/PortCell.cpp
	binding/ProcedureCell.cpp
	binding/RecordCell.cpp
	binding/RecordLikeCell.cpp
	binding/StringCell.cpp
	binding/SymbolCell.cpp
	binding/VectorCell.cpp
	core/World.cpp
	core/constinstances.cpp
	core/dynamic.cpp
	core/eqv.cpp
	core/error.cpp
	core/init.cpp
	core/io.cpp
	core/recorddata.cpp
	core/vector.cpp
	dynamic/Continuation.cpp
	dynamic/State.cpp
	dynamic/EscapeProcedureCell.cpp
	dynamic/ParameterProcedureCell.cpp
	dynamic/init.cpp
	platform/memory.cpp
	port/StandardInputPort.cpp
	reader/ReadErrorException.cpp
	reader/DatumReader.cpp
	stdlib/arithmetic.cpp
	stdlib/boolean.cpp
	stdlib/bytevector.cpp
	stdlib/char.cpp
	stdlib/control.cpp
	stdlib/dynamic.cpp
	stdlib/exception.cpp
	stdlib/file.cpp
	stdlib/inexact.cpp
	stdlib/input.cpp
	stdlib/list.cpp
	stdlib/map.cpp
	stdlib/number.cpp
	stdlib/numberio.cpp
	stdlib/port.cpp
	stdlib/output.cpp
	stdlib/process.cpp
	stdlib/string.cpp
	stdlib/symbol.cpp
	stdlib/vector.cpp
	unicode/UnicodeChar.cpp
	unicode/utf8.cpp
	unicode/utf8/InvalidByteSequenceException.cpp
	util/SharedByteArray.cpp
	util/utf8ExceptionToSchemeError.cpp
	writer/DisplayDatumWriter.cpp
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
if (${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Werror")
endif()

# Enable C++11
ADD_DEFINITIONS(-std=c++11)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	# Mac OS X < 10.9 used an old version of libstdc++ as the default C++ stdlib. It did not support C++11.
	# Explicitly use libc++ to enable full C++11 support
	# This workaround and the matching one in codegen can be removed once Mac OS X 10.8 is unsupported.
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

# Force colour output so Clang++ prints pretty errors in Ninja
if (${CMAKE_GENERATOR} STREQUAL "Ninja" AND ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
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
	datumreader
	displaydatumwriter
	externalformdatumwriter
	implicitsharing
	flonum
	listelement
	properlist
	sharedbytearray
	string
	unicodechar
	utf8
	vector)

foreach( test_name ${ALL_TEST_NAMES} )
	add_executable(test-${test_name} tests/test-${test_name}.cpp)
	target_link_libraries(test-${test_name} lliby ${CMAKE_THREAD_LIBS_INIT})

	add_test(${test_name} test-${test_name})
endforeach()
