#!/bin/bash

# If anything fails we should fail
set -e

BUILD_DIR=build/

rm -Rf $BUILD_DIR
mkdir $BUILD_DIR
cd $BUILD_DIR

# List of configurations of the runtime to test
RUNTIME_CONFIGURATIONS=("-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=yes"
                        "-DCMAKE_BUILD_TYPE=Debug -DENABLE_GC_DEBUGGING=no"
                        "-DCMAKE_BUILD_TYPE=Release")

for config in "${RUNTIME_CONFIGURATIONS[@]}"
do
	cmake -GNinja $config ../runtime
	ninja

	# Run time runtime tests
	ninja test

	cd ..

	# Run the Scala tests
	sbt test

	cd -
done
