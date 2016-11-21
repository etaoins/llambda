# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(llcore
	actor/ActorContext.cpp
	actor/Mailbox.cpp
	actor/Message.cpp
	actor/PoisonPillCell.cpp
	actor/Runner.cpp
	actor/cloneCell.cpp
	alloc/CellRootList.cpp
	alloc/Finalizer.cpp
	alloc/Heap.cpp
	alloc/MemoryBlock.cpp
	alloc/allocator.cpp
	alloc/collector.cpp
	binding/BytevectorCell.cpp
	binding/CharCell.cpp
	binding/AnyCell.cpp
	binding/DynamicStateCell.cpp
	binding/ErrorObjectCell.cpp
	binding/HashMapCell.cpp
	binding/ListElementCell.cpp
	binding/MailboxCell.cpp
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
	binding/generated/ErrorCategory.cpp
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
	dynamic/State.cpp
	dynamic/ParameterProcedureCell.cpp
	dynamic/init.cpp
	hash/DatumHash.cpp
	hash/DatumHashTree.cpp
	hash/SharedByteHash.cpp
	platform/memory.cpp
	platform/time.cpp
	port/StandardInputPort.cpp
	reader/ReadErrorException.cpp
	reader/DatumReader.cpp
	sched/Dispatcher.cpp
	sched/TimerList.cpp
	unicode/utf8.cpp
	unicode/utf8/InvalidByteSequenceException.cpp
	util/portCellToStream.cpp
	util/rangeAssertions.cpp
	util/utf8ExceptionToSchemeError.cpp
	writer/DisplayDatumWriter.cpp
	writer/ExternalFormDatumWriter.cpp
)

add_library(ll_llambda_actor
	stdlib/llambda/actor/actor.cpp
)

add_library(ll_llambda_list
	stdlib/llambda/list/list.cpp
)

add_library(ll_llambda_error
	stdlib/llambda/error/error.cpp
)

add_library(ll_llambda_hashmap
	stdlib/llambda/hash-map/hash-map.cpp
)


add_library(ll_llambda_random
	stdlib/llambda/random/random.cpp
)

add_library(ll_llambda_time
	stdlib/llambda/time/time.cpp
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

# Enable optimisations by default
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

# Force colour output so Clang++ prints pretty errors in Ninja
if (${CMAKE_GENERATOR} STREQUAL "Ninja" AND ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
	ADD_DEFINITIONS(-fcolor-diagnostics)
endif()

# GC debugging mode
set(ENABLE_GC_DEBUGGING "no" CACHE STRING "Enable extra garbage collector sanity checks at a severe performance penalty")
if (${ENABLE_GC_DEBUGGING} STREQUAL "yes")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_LLIBY_CHECK_LEAKS")

	if (NOT (CMAKE_SIZEOF_VOID_P LESS 8))
		# Only enable _LLIBY_ALWAYS_GC if we have enough address space
		# Additionally, 32bit systems are generally slower and can't afford the overhead of ALWAYS_GC
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_LLIBY_ALWAYS_GC")
	endif()
endif()

# Build the fuzzer
set(ENABLE_DATUM_FUZZ_DRIVER "no" CACHE STRING "Build a driver program for the datum reader and writer suitable for use with afl-fuzz")
if (${ENABLE_DATUM_FUZZ_DRIVER} STREQUAL "yes")
	add_executable(datum-fuzz-driver
		tools/datum-fuzz-driver.cpp
	)
	target_link_libraries(datum-fuzz-driver llcore ${CMAKE_THREAD_LIBS_INIT})
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
	datumhash
	datumhashtree
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

# Write metadata information for the Llambda compiler
set(SEPARATE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
separate_arguments(SEPARATE_CXX_FLAGS)

file(WRITE "${CMAKE_BINARY_DIR}/required-clang-flags" "${CMAKE_THREAD_LIBS_INIT};${SEPARATE_CXX_FLAGS}")

execute_process(
	COMMAND echo "int main(int argc, char *argv[]) { return 0; }"
	COMMAND clang++ -x c++ - ${SEPARATE_CXX_FLAGS} -emit-llvm -S -o -
	COMMAND sed -n "/^target /p"
	OUTPUT_FILE "llvm-target"
)
