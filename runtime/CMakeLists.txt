# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(lliby
	alloc/allocator.cpp
	binding/BoxedDatum.cpp
	binding/ProcedureValue.cpp
	core/constinstances.cpp
	core/fatal.cpp
	core/init.cpp
	stdlib/generated/predicates.cpp
	)

include_directories(${lliby_SOURCE_DIR})

# Create compile_commands.json. Clang tools like this
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# Enable optimizations by default
if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

# Be strict about warnings
set(CMAKE_CXX_FLAGS "-Werror -Wall")

# Enable C++11
ADD_DEFINITIONS(
	-std=c++11 
	)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	# Mac OS X's libstdc++ is too old for C++11 support
	# Explicitly use libc++
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

# Add tests
enable_testing()

set(ALL_TEST_NAMES
	constinstances)

foreach( test_name ${ALL_TEST_NAMES} )
	add_executable(tests/bin/test-${test_name} tests/test-${test_name}.cpp)
	target_link_libraries(tests/bin/test-${test_name} lliby)

	add_test(${test_name} tests/bin/test-${test_name})
endforeach()
