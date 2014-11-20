#!/bin/sh

# If anything fails we should fail
set -e

BUILD_DIR=build/

rm -Rf $BUILD_DIR
mkdir $BUILD_DIR
cd $BUILD_DIR

test_configuration() {
	cmake -GNinja $1 ../runtime
	ninja

	# Run time runtime tests
	ninja test

	cd ..

	# Run the Scala tests
	sbt test

	cd -
}

# Test each configuration
test_configuration "-DCMAKE_BUILD_TYPE=Release"
test_configuration "-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=no"
test_configuration "-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=yes"
