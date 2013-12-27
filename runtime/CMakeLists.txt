# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(lliby
	alloc/allocator.cpp
	binding/BytevectorCell.cpp
	binding/DatumCell.cpp
	binding/ListElementCell.cpp
	binding/ProcedureCell.cpp
	binding/RecordLikeCell.cpp
	binding/StringLikeCell.cpp
	binding/StringCell.cpp
	binding/VectorCell.cpp
	core/constinstances.cpp
	core/fatal.cpp
	core/init.cpp
	core/recorddata.cpp
	stdlib/generated/predicates.cpp
	stdlib/boolean.cpp
	stdlib/bytevector.cpp
	stdlib/character.cpp
	stdlib/control.cpp
	stdlib/eqv.cpp
	stdlib/list.cpp
	stdlib/number.cpp
	stdlib/output.cpp
	stdlib/string.cpp
	stdlib/symbol.cpp
	stdlib/vector.cpp
	unicode/UnicodeChar.cpp
	writer/ExternalFormDatumWriter.cpp
	)

include_directories(${lliby_SOURCE_DIR})

# Create compile_commands.json. Clang tools like this
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# Enable optimizations by default
if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

# Be strict about warnings
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -Wall")

# Enable C++11 and disable exceptions
# Exceptions have two issues:
# - C++ exceptions will not propagate correctly through Scheme code 
# - When operator new is used Clang will generate unwind code referencing 
#   operator delete. This has been explicitly removed from DatumCell so it
#   causes link errors whenever operator new is used on a DatumCell subclass
ADD_DEFINITIONS(
	-std=c++11 
	-fno-exceptions
	)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	# Mac OS X's libstdc++ is too old for C++11 support
	# Explicitly use libc++
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

# Add tests
enable_testing()

set(ALL_TEST_NAMES
	bytevector
	constinstances
	inexactrational
	listelement
	string
	unicodechar
	vector
	externalformdatumwriter
	properlist)

foreach( test_name ${ALL_TEST_NAMES} )
	add_executable(tests/bin/test-${test_name} tests/test-${test_name}.cpp)
	target_link_libraries(tests/bin/test-${test_name} lliby)

	add_test(${test_name} tests/bin/test-${test_name})
endforeach()
