#!/bin/sh

# If anything fails we should fail
set -e

if [[ $# -eq 0 ]] ; then
    echo "Usage: $0 output-dir"
    exit 1
fi

BUILD_DIR=build/
BUIlD_CONFIGURATION="-DCMAKE_BUILD_TYPE=Before"

OUTPUT_DIR=$1
BEFORE_DIR=$OUTPUT_DIR/before
AFTER_DIR=$OUTPUT_DIR/after
DELTA_PATCH=$OUTPUT_DIR/delta.patch

rebuild_runtime() {
	rm -Rf $BUILD_DIR
	mkdir $BUILD_DIR
	cd $BUILD_DIR

	cmake -GNinja $BUIlD_CONFIGURATION ../runtime
	ninja

	cd ..
}

assemble_llvm_ir() {
	for file in $1/*.ll
	do
		opt -O2 -- "$file" | llc -O2 -o "$file.s"
	done
}

build_and_assemble() {
	rebuild_runtime
	mkdir $1
	LLAMBDA_FUNCTIONAL_TEST_ONLY_OPTIMISED=1 LLAMBDA_FUNCTIONAL_TEST_LLVMIR_DIR=$1 sbt "testOnly io.llambda.compiler.functional.*"
	assemble_llvm_ir $1
}

mkdir -p $OUTPUT_DIR

# Be cautious and only delete things we create
rm -Rf $BEFORE_DIR
rm -Rf $AFTER_DIR
rm -f $DELTA_PATCH

git stash push -u -m "functional-asm-delta"
build_and_assemble $BEFORE_DIR
git stash pop

build_and_assemble $AFTER_DIR

diff -u $BEFORE_DIR $AFTER_DIR > $DELTA_PATCH
