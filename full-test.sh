#!/bin/sh

# If anything fails we should fail
set -e

BUILD_DIR=build/
FUNCTIONAL_TEST_PATTERN="io.llambda.compiler.functional.*"

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
	sbt "testOnly $2"

	cd -
}

# Test each configuration
test_configuration "-DCMAKE_BUILD_TYPE=Release" "*"
test_configuration "-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=no" $FUNCTIONAL_TEST_PATTERN
test_configuration "-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=yes" $FUNCTIONAL_TEST_PATTERN
