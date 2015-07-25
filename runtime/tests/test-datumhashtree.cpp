#include "hash/DatumHashTree.h"

#include <random>

#include "alloc/cellref.h"
#include "alloc/StrongRefVector.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"
#include "binding/BooleanCell.h"

#include "writer/ExternalFormDatumWriter.cpp"
#include "core/init.h"
#include "core/World.h"
#include "assertions.h"
#include "stubdefinitions.h"

using namespace lliby;

namespace
{

DatumHashTree* pivotTree(DatumHashTree *&treeLoc, DatumHashTree *tree)
{
	DatumHashTree::unref(treeLoc);
	treeLoc = tree;

	return tree;
}

void testBasicImmutable(World &world)
{
	alloc::ExactIntegerRef intZero(world, ExactIntegerCell::fromValue(world, 0));
	alloc::ExactIntegerRef intOne(world, ExactIntegerCell::fromValue(world, 1));
	alloc::ExactIntegerRef intTwo(world, ExactIntegerCell::fromValue(world, 2));
	alloc::ExactIntegerRef intThree(world, ExactIntegerCell::fromValue(world, 3));

	alloc::StringRef stringZero(world, StringCell::fromUtf8StdString(world, "0"));
	alloc::StringRef stringOne(world, StringCell::fromUtf8StdString(world, "1"));
	alloc::StringRef stringTwo(world, StringCell::fromUtf8StdString(world, "2"));
	alloc::StringRef stringThree(world, StringCell::fromUtf8StdString(world, "3"));

	DatumHashTree *tree = DatumHashTree::createEmpty();
	DatumHashTree *emptyTree = DatumHashTree::ref(tree);

	auto verifyEmptyTree = [&] ()
	{
		// Nothing should be in the hash table
		ASSERT_EQUAL(DatumHashTree::size(emptyTree), 0);
		ASSERT_NULL(DatumHashTree::find(emptyTree, stringZero));
		ASSERT_NULL(DatumHashTree::find(emptyTree, stringOne));
		ASSERT_NULL(DatumHashTree::find(emptyTree, stringTwo));
		ASSERT_NULL(DatumHashTree::find(emptyTree, stringThree));

		DatumHashTree::walk(emptyTree, [&] (AnyCell *key, AnyCell *value)
		{
			ASSERT_TRUE(false);
		});
	};

	verifyEmptyTree();

	// Add a single mapping
	pivotTree(tree, DatumHashTree::assoc(tree, stringZero, intZero));
	DatumHashTree *oneValueTree = DatumHashTree::ref(tree);

	auto verifyOneValueTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(oneValueTree), 1);
		ASSERT_EQUAL(DatumHashTree::find(oneValueTree, stringZero), intZero.data());
		ASSERT_NULL(DatumHashTree::find(oneValueTree, stringOne));
		ASSERT_NULL(DatumHashTree::find(oneValueTree, stringTwo));
		ASSERT_NULL(DatumHashTree::find(oneValueTree, stringThree));
	};

	verifyOneValueTree();

	// Add a second mapping
	pivotTree(tree, DatumHashTree::assoc(tree, stringOne, intOne));
	DatumHashTree *twoValueTree = DatumHashTree::ref(tree);

	auto verifyTwoValueTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(twoValueTree), 2);
		ASSERT_EQUAL(DatumHashTree::find(twoValueTree, stringZero), intZero.data());
		ASSERT_EQUAL(DatumHashTree::find(twoValueTree, stringOne), intOne.data());
		ASSERT_NULL(DatumHashTree::find(twoValueTree, stringTwo));
		ASSERT_NULL(DatumHashTree::find(twoValueTree, stringThree));
	};

	verifyTwoValueTree();

	// Add a third mapping
	pivotTree(tree, DatumHashTree::assoc(tree, stringTwo, intTwo));
	DatumHashTree *threeValueTree = DatumHashTree::ref(tree);

	auto verifyThreeValueTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(threeValueTree), 3);
		ASSERT_EQUAL(DatumHashTree::find(threeValueTree, stringZero), intZero.data());
		ASSERT_EQUAL(DatumHashTree::find(threeValueTree, stringOne), intOne.data());
		ASSERT_EQUAL(DatumHashTree::find(threeValueTree, stringTwo), intTwo.data());
		ASSERT_NULL(DatumHashTree::find(threeValueTree, stringThree));
	};

	verifyThreeValueTree();

	// Add a forth mapping
	pivotTree(tree, DatumHashTree::assoc(tree, stringThree, intThree));
	DatumHashTree *fourValueTree = DatumHashTree::ref(tree);

	auto verifyFourValueTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(fourValueTree), 4);
		ASSERT_EQUAL(DatumHashTree::find(fourValueTree, stringZero), intZero.data());
		ASSERT_EQUAL(DatumHashTree::find(fourValueTree, stringOne), intOne.data());
		ASSERT_EQUAL(DatumHashTree::find(fourValueTree, stringTwo), intTwo.data());
		ASSERT_EQUAL(DatumHashTree::find(fourValueTree, stringThree), intThree.data());

		bool seenValues[4] = {false};
		DatumHashTree::walk(fourValueTree, [&] (AnyCell *key, AnyCell *value)
		{
			std::int64_t intValue = cell_cast<ExactIntegerCell>(value)->value();

			ASSERT_TRUE(intValue < 4);
			ASSERT_FALSE(seenValues[intValue]);

			seenValues[intValue] = true;
		});

		for(std::size_t i = 0; i < 4; i++)
		{
			ASSERT_TRUE(seenValues[i]);
		}
	};

	verifyFourValueTree();

	// Swap the mappings
	pivotTree(tree, DatumHashTree::assoc(tree, stringZero, intThree));
	pivotTree(tree, DatumHashTree::assoc(tree, stringOne, intTwo));
	pivotTree(tree, DatumHashTree::assoc(tree, stringTwo, intOne));
	pivotTree(tree, DatumHashTree::assoc(tree, stringThree, intZero));

	DatumHashTree *swappedTree = DatumHashTree::ref(tree);

	auto verifySwappedTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(fourValueTree), 4);
		ASSERT_EQUAL(DatumHashTree::find(swappedTree, stringZero), intThree.data());
		ASSERT_EQUAL(DatumHashTree::find(swappedTree, stringOne), intTwo.data());
		ASSERT_EQUAL(DatumHashTree::find(swappedTree, stringTwo), intOne.data());
		ASSERT_EQUAL(DatumHashTree::find(swappedTree, stringThree), intZero.data());

		bool seenValues[4] = {false};
		DatumHashTree::walk(fourValueTree, [&] (AnyCell *key, AnyCell *value)
		{
			std::int64_t intValue = cell_cast<ExactIntegerCell>(value)->value();

			ASSERT_TRUE(intValue < 4);
			ASSERT_FALSE(seenValues[intValue]);

			seenValues[intValue] = true;
		});

		for(std::size_t i = 0; i < 4; i++)
		{
			ASSERT_TRUE(seenValues[i]);
		}
	};

	verifySwappedTree();

	// Remove a key not in the hash tree
	auto newTree = DatumHashTree::without(tree, intZero.data());
	ASSERT_EQUAL(tree, newTree);
	DatumHashTree::unref(newTree);

	// Remove the first mapping
	pivotTree(tree, DatumHashTree::without(tree, stringZero));
	DatumHashTree *oneRemovedTree = DatumHashTree::ref(tree);

	auto verifyOneRemovedTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(oneRemovedTree), 3);
		ASSERT_NULL(DatumHashTree::find(oneRemovedTree, stringZero));
		ASSERT_EQUAL(DatumHashTree::find(oneRemovedTree, stringOne), intTwo.data());
		ASSERT_EQUAL(DatumHashTree::find(oneRemovedTree, stringTwo), intOne.data());
		ASSERT_EQUAL(DatumHashTree::find(oneRemovedTree, stringThree), intZero.data());
	};

	verifyOneRemovedTree();

	// Remove the second mapping
	pivotTree(tree, DatumHashTree::without(tree, stringOne));
	DatumHashTree *twoRemovedTree = DatumHashTree::ref(tree);

	auto verifyTwoRemovedTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(twoRemovedTree), 2);
		ASSERT_NULL(DatumHashTree::find(twoRemovedTree, stringZero));
		ASSERT_NULL(DatumHashTree::find(twoRemovedTree, stringOne));
		ASSERT_EQUAL(DatumHashTree::find(twoRemovedTree, stringTwo), intOne.data());
		ASSERT_EQUAL(DatumHashTree::find(twoRemovedTree, stringThree), intZero.data());
	};

	verifyTwoRemovedTree();

	// Remove the third mapping
	pivotTree(tree, DatumHashTree::without(tree, stringTwo));
	DatumHashTree *threeRemovedTree = DatumHashTree::ref(tree);

	auto verifyThreeRemovedTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(threeRemovedTree), 1);
		ASSERT_NULL(DatumHashTree::find(threeRemovedTree, stringZero));
		ASSERT_NULL(DatumHashTree::find(threeRemovedTree, stringOne));
		ASSERT_NULL(DatumHashTree::find(threeRemovedTree, stringTwo));
		ASSERT_EQUAL(DatumHashTree::find(threeRemovedTree, stringThree), intZero.data());
	};

	verifyThreeRemovedTree();

	// Remove the fourth mapping
	pivotTree(tree, DatumHashTree::without(tree, stringThree));
	DatumHashTree *fourRemovedTree = DatumHashTree::ref(tree);

	auto verifyFourRemovedTree = [&]
	{
		ASSERT_EQUAL(DatumHashTree::size(fourRemovedTree), 0);
		ASSERT_NULL(DatumHashTree::find(fourRemovedTree, stringZero));
		ASSERT_NULL(DatumHashTree::find(fourRemovedTree, stringOne));
		ASSERT_NULL(DatumHashTree::find(fourRemovedTree, stringTwo));
		ASSERT_NULL(DatumHashTree::find(fourRemovedTree, stringThree));

		DatumHashTree::walk(fourRemovedTree, [&] (AnyCell *key, AnyCell *value)
		{
			ASSERT_TRUE(false);
		});
	};

	verifyFourRemovedTree();

	// Check all of the trees again
	verifyEmptyTree();
	verifyOneValueTree();
	verifyTwoValueTree();
	verifyThreeValueTree();
	verifyFourValueTree();
	verifySwappedTree();
	verifyOneRemovedTree();
	verifyTwoRemovedTree();
	verifyThreeRemovedTree();
	verifyFourRemovedTree();

	// Unref all of the trees
	DatumHashTree::unref(emptyTree);
	verifyOneValueTree();

	DatumHashTree::unref(oneValueTree);
	DatumHashTree::unref(twoValueTree);
	DatumHashTree::unref(threeValueTree);
	DatumHashTree::unref(fourValueTree);
	DatumHashTree::unref(swappedTree);
	DatumHashTree::unref(oneRemovedTree);
	DatumHashTree::unref(twoRemovedTree);
	DatumHashTree::unref(threeRemovedTree);
	DatumHashTree::unref(fourRemovedTree);

	DatumHashTree::unref(tree);

	// Ensure we didn't leak any instances
	ASSERT_EQUAL(DatumHashTree::instanceCount(), 0);
}

void testLargeImmutableTree(World &world)
{
	static const std::size_t testIntegerCount = 2000;

	alloc::StrongRefVector<ExactIntegerCell> intVector(world);
	alloc::StrongRefVector<FlonumCell> flonumVector(world);
	intVector.reserve(testIntegerCount);

	std::mt19937 gen;
	gen.seed(0);

	std::uniform_int_distribution<DatumHash::ResultType> distribution;

	for(std::size_t i = 0; i < testIntegerCount; i++)
	{
		auto randomNumber = distribution(gen);

		// These should have colliding hash codes
		intVector.push_back(ExactIntegerCell::fromValue(world, randomNumber));
		intVector.push_back(ExactIntegerCell::fromValue(world, randomNumber + (1ULL << 32)));

		flonumVector.push_back(FlonumCell::fromValue(world, randomNumber));
	}

	DatumHash hasher;
	ASSERT_EQUAL(hasher(intVector[0]), hasher(intVector[1]));

	DatumHashTree *tree = DatumHashTree::createEmpty();

	for(auto intCell : intVector)
	{
		pivotTree(tree, DatumHashTree::assoc(tree, intCell, BooleanCell::trueInstance()));
	}

	DatumHashTree *allIntegerTree = DatumHashTree::ref(tree);

	auto verifyAllIntegerTree = [&] ()
	{
		ASSERT_EQUAL(DatumHashTree::size(allIntegerTree), testIntegerCount * 2);

		// Ensure none of the float values are there
		for(auto flonumCell : flonumVector)
		{
			ASSERT_NULL(DatumHashTree::find(allIntegerTree, flonumCell));
		}

		// Ensure all the values are there
		for(auto intCell : intVector)
		{
			ASSERT_EQUAL(DatumHashTree::find(allIntegerTree, intCell), BooleanCell::trueInstance());
		}
	};

	verifyAllIntegerTree();

	// Convert the odd values to false
	for(auto intCell : intVector)
	{
		if (intCell->value() % 2)
		{
			pivotTree(tree, DatumHashTree::assoc(tree, intCell, BooleanCell::falseInstance()));
		}
	}

	DatumHashTree *oddFalseTree = DatumHashTree::ref(tree);

	auto verifyOddFalseTree = [&] ()
	{
		ASSERT_EQUAL(DatumHashTree::size(oddFalseTree), testIntegerCount * 2);

		for(auto intCell : intVector)
		{
			if (intCell->value() % 2)
			{
				ASSERT_EQUAL(DatumHashTree::find(oddFalseTree, intCell), BooleanCell::falseInstance());
			}
			else
			{
				ASSERT_EQUAL(DatumHashTree::find(oddFalseTree, intCell), BooleanCell::trueInstance());
			}
		}

		DatumHashTree::walk(oddFalseTree, [&] (AnyCell *key, AnyCell *value)
		{
			std::int64_t intValue = cell_cast<ExactIntegerCell>(key)->value();

			if (intValue % 2)
			{
				ASSERT_EQUAL(value, BooleanCell::falseInstance());
			}
			else
			{
				ASSERT_EQUAL(value, BooleanCell::trueInstance());
			}
		});
	};

	verifyOddFalseTree();

	// Remove the even values
	for(auto intCell : intVector)
	{
		if ((intCell->value() % 2) == 0)
		{
			pivotTree(tree, DatumHashTree::without(tree, intCell));
		}
	}

	// And a bunch of values that don't exist
	for(auto flonumCell : flonumVector)
	{
		pivotTree(tree, DatumHashTree::without(tree, flonumCell));
	}

	DatumHashTree *removedEvenTree = DatumHashTree::ref(tree);

	auto verifyRemovedEvenTree = [&] ()
	{
		for(auto intCell : intVector)
		{
			if (intCell->value() % 2)
			{
				ASSERT_EQUAL(DatumHashTree::find(removedEvenTree, intCell), BooleanCell::falseInstance());
			}
			else
			{
				ASSERT_NULL(DatumHashTree::find(removedEvenTree, intCell));
			}
		}

		DatumHashTree::walk(removedEvenTree, [&] (AnyCell *key, AnyCell *value)
		{
			std::int64_t intValue = cell_cast<ExactIntegerCell>(key)->value();
			ASSERT_TRUE(intValue % 2);
		});
	};

	verifyRemovedEvenTree();

	// Now remove the odd values
	for(auto intCell : intVector)
	{
		if ((intCell->value() % 2) == 1)
		{
			pivotTree(tree, DatumHashTree::without(tree, intCell));
		}
	}

	DatumHashTree *removedAllTree = DatumHashTree::ref(tree);

	auto verifyRemovedAllTree = [&] ()
	{
		ASSERT_EQUAL(DatumHashTree::size(removedAllTree), 0);

		for(auto intCell : intVector)
		{
			ASSERT_NULL(DatumHashTree::find(removedAllTree, intCell));
		}

		DatumHashTree::walk(removedAllTree, [&] (AnyCell *key, AnyCell *value)
		{
			ASSERT_TRUE(false);
		});
	};

	verifyAllIntegerTree();
	verifyOddFalseTree();
	verifyRemovedEvenTree();
	verifyRemovedAllTree();

	DatumHashTree::unref(allIntegerTree);
	DatumHashTree::unref(oddFalseTree);
	DatumHashTree::unref(removedEvenTree);
	DatumHashTree::unref(tree);
	ASSERT_EQUAL(DatumHashTree::instanceCount(), 0);
}

void testToFromAssocList(World &world)
{
	alloc::ExactIntegerRef intZero(world, ExactIntegerCell::fromValue(world, 0));
	alloc::ExactIntegerRef intOne(world, ExactIntegerCell::fromValue(world, 1));
	alloc::ExactIntegerRef intTwo(world, ExactIntegerCell::fromValue(world, 2));
	alloc::ExactIntegerRef intThree(world, ExactIntegerCell::fromValue(world, 2));
	alloc::ExactIntegerRef intFour(world, ExactIntegerCell::fromValue(world, 4));

	alloc::StringRef stringZero(world, StringCell::fromUtf8StdString(world, "0"));
	alloc::StringRef stringOne(world, StringCell::fromUtf8StdString(world, "1"));
	alloc::StringRef stringTwo(world, StringCell::fromUtf8StdString(world, "2"));
	alloc::StringRef stringThree(world, StringCell::fromUtf8StdString(world, "3"));
	alloc::StringRef stringFour(world, StringCell::fromUtf8StdString(world, "4"));

	alloc::PairRef pairZero(world, PairCell::createInstance(world, stringZero, intFour));
	alloc::PairRef pairOne(world, PairCell::createInstance(world, stringOne, intOne));
	alloc::PairRef pairTwo(world, PairCell::createInstance(world, stringTwo, intTwo));
	alloc::PairRef pairThree(world, PairCell::createInstance(world, stringThree, intThree));
	alloc::PairRef pairFour(world, PairCell::createInstance(world, stringFour, intFour));
	alloc::PairRef pairFive(world, PairCell::createInstance(world, stringZero, intZero));

	auto assocList = ProperList<PairCell>::create(world, {pairZero, pairOne, pairTwo, pairThree, pairFour, pairFive});

	DatumHashTree *tree = DatumHashTree::fromAssocList(assocList);

	ASSERT_EQUAL(DatumHashTree::size(tree), 5);
	ASSERT_EQUAL(DatumHashTree::find(tree, stringZero), intZero.data());
	ASSERT_EQUAL(DatumHashTree::find(tree, stringOne), intOne.data());
	ASSERT_EQUAL(DatumHashTree::find(tree, stringTwo), intTwo.data());
	ASSERT_EQUAL(DatumHashTree::find(tree, stringThree), intThree.data());
	ASSERT_EQUAL(DatumHashTree::find(tree, stringFour), intFour.data());

	DatumHashTree::unref(tree);
	ASSERT_EQUAL(DatumHashTree::instanceCount(), 0);
}

void testAll(World &world)
{
	testBasicImmutable(world);
	testLargeImmutableTree(world);
	testToFromAssocList(world);
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
