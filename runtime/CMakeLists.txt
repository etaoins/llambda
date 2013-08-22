# This is the version used to build this file
# It hasn't been tested on earlier versions of CMake but it may work
cmake_minimum_required (VERSION 2.8)

project (lliby)
add_library(lliby
	alloc/allocator.cpp
	binding/BoxedDatum.cpp
	binding/ProcedureValue.cpp
	core/fatal.cpp
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
	ADD_DEFINITIONS(-stdlib=libc++)
endif()
