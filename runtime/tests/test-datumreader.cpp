#include <sstream>

#include "core/init.h"
#include "core/World.h"

#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"

#include "reader/DatumReader.h"
#include "writer/ExternalFormDatumWriter.h"

#include "alloc/cellref.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

#define ASSERT_PARSES(datumString, expectedRaw) \
{ \
	std::istringstream inputStream(datumString); \
	DatumReader reader(world, inputStream); \
	alloc::StrongRef<AnyCell> expected(world, expectedRaw); \
	\
	AnyCell *actual = reader.parse(); \
	\
	if (!actual->isEqual(expected)) \
	{ \
		ExternalFormDatumWriter writer(std::cerr); \
		std::cerr << "\"" << datumString << "\" did not parse as expected value \""; \
		writer.render(expected); \
		std::cerr << "\"; instead parsed as \""; \
		writer.render(actual); \
		std::cerr << "\" at line " << __LINE__ << std::endl; \
		\
		exit(-1); \
	} \
}

void testEmptyInput(World &world)
{
	ASSERT_PARSES("", EofObjectCell::instance());
}

void testBooleans(World &world)
{
	ASSERT_PARSES("#t", BooleanCell::trueInstance());
	ASSERT_PARSES("#true", BooleanCell::trueInstance());

	ASSERT_PARSES("#f", BooleanCell::falseInstance());
	ASSERT_PARSES("#false", BooleanCell::falseInstance());
}

void testIntegers(World &world)
{
	ASSERT_PARSES("0", ExactIntegerCell::fromValue(world, 0));
	ASSERT_PARSES("000", ExactIntegerCell::fromValue(world, 0));
	ASSERT_PARSES("1000", ExactIntegerCell::fromValue(world, 1000));
	ASSERT_PARSES("-1000", ExactIntegerCell::fromValue(world, -1000));

	ASSERT_PARSES("3.", ExactIntegerCell::fromValue(world, 3));

	ASSERT_PARSES("#b111", ExactIntegerCell::fromValue(world, 7));
	ASSERT_PARSES("#B-1000", ExactIntegerCell::fromValue(world, -8));

	ASSERT_PARSES("#O1234", ExactIntegerCell::fromValue(world, 668));
	ASSERT_PARSES("#o-010", ExactIntegerCell::fromValue(world, -8));

	ASSERT_PARSES("#D1234", ExactIntegerCell::fromValue(world, 1234));
	ASSERT_PARSES("#d-010", ExactIntegerCell::fromValue(world, -10));

	ASSERT_PARSES("#Xdead", ExactIntegerCell::fromValue(world, 57005));
	ASSERT_PARSES("#x-b00b5", ExactIntegerCell::fromValue(world, -721077));

	ASSERT_PARSES("9007199254740993", ExactIntegerCell::fromValue(world, 9007199254740993LL));
}

void testReals(World &world)
{
	ASSERT_PARSES("0.0", FlonumCell::fromValue(world, 0.0));
	ASSERT_PARSES("33.337", FlonumCell::fromValue(world, 33.337));
	ASSERT_PARSES("-0100.0", FlonumCell::fromValue(world, -100.0));

	ASSERT_PARSES("2/5", FlonumCell::fromValue(world, 0.4));
	ASSERT_PARSES("+20/50", FlonumCell::fromValue(world, 0.4));
	ASSERT_PARSES("-20/2", FlonumCell::fromValue(world, -10.0));

	ASSERT_PARSES("+inf.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-inf.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+INF.0", FlonumCell::positiveInfinity(world));
	ASSERT_PARSES("-INF.0", FlonumCell::negativeInfinity(world));

	ASSERT_PARSES("+NaN.0", FlonumCell::NaN(world));
	ASSERT_PARSES("-NaN.0", FlonumCell::NaN(world));
}

void testAll(World &world)
{
	testEmptyInput(world);
	testBooleans(world);
	testIntegers(world);
	testReals(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
