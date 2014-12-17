# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(llcore
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
	binding/SharedByteArray.cpp
	binding/StringCell.cpp
	binding/SymbolCell.cpp
	binding/VectorCell.cpp
	core/World.cpp
	core/alloc.cpp
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
	platform/time.cpp
	port/StandardInputPort.cpp
	reader/ReadErrorException.cpp
	reader/DatumReader.cpp
	unicode/utf8.cpp
	unicode/utf8/InvalidByteSequenceException.cpp
	util/portCellToStream.cpp
	util/utf8ExceptionToSchemeError.cpp
	writer/DisplayDatumWriter.cpp
	writer/ExternalFormDatumWriter.cpp
)

add_library(ll_llambda_list
	stdlib/llambda/list/list.cpp
)

add_library(ll_llambda_error
	stdlib/llambda/error/error.cpp
)

add_library(ll_scheme_base
	stdlib/scheme/base/arithmetic.cpp
	stdlib/scheme/base/boolean.cpp
	stdlib/scheme/base/bytevector.cpp
	stdlib/scheme/base/char.cpp
	stdlib/scheme/base/control.cpp
	stdlib/scheme/base/dynamic.cpp
	stdlib/scheme/base/exception.cpp
	stdlib/scheme/base/input.cpp
	stdlib/scheme/base/list.cpp
	stdlib/scheme/base/map.cpp
	stdlib/scheme/base/number.cpp
	stdlib/scheme/base/numberio.cpp
	stdlib/scheme/base/port.cpp
	stdlib/scheme/base/output.cpp
	stdlib/scheme/base/string.cpp
	stdlib/scheme/base/symbol.cpp
	stdlib/scheme/base/vector.cpp
)

add_library(ll_scheme_char
	stdlib/scheme/char/char.cpp
	ucd/ucd.cpp
)

add_library(ll_scheme_inexact
	stdlib/scheme/inexact/inexact.cpp
)

add_library(ll_scheme_file
	stdlib/scheme/file/file.cpp
)

add_library(ll_scheme_processcontext
	stdlib/scheme/process-context/process.cpp
)

add_library(ll_scheme_read
	stdlib/scheme/read/read.cpp
)

add_library(ll_scheme_time
	stdlib/scheme/time/time.cpp
)

add_library(ll_scheme_write
	stdlib/scheme/write/write.cpp
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
	symbol
	ucd
	utf8
	vector)

foreach( test_name ${ALL_TEST_NAMES} )
	add_executable(test-${test_name} tests/test-${test_name}.cpp)
	target_link_libraries(test-${test_name} llcore ll_scheme_char ${CMAKE_THREAD_LIBS_INIT})

	add_test(${test_name} test-${test_name})
endforeach()
