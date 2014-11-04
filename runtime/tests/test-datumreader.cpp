#include <sstream>

#include "core/init.h"
#include "core/World.h"

#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"

#include "reader/DatumReader.h"
#include "writer/ExternalFormDatumWriter.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

#define ASSERT_PARSES(datumString, expected) \
{ \
	std::istringstream inputStream(datumString); \
	DatumReader reader(world, inputStream); \
	\
	AnyCell *actual = reader.parse(); \
	\
	if (actual != expected) \
	{ \
		ExternalFormDatumWriter writer(std::cerr); \
		std::cerr << "\"" << datumString << "\" did not parse as expected value \""; \
		writer.render(expected); \
		std::cerr << "\"; instead parsed as \""; \
		writer.render(actual); \
		std::cerr << "\"" << std::endl; \
		\
		exit(-1); \
	} \
}

void testEmptyInput(World &world)
{
	ASSERT_PARSES("", EofObjectCell::instance());
}

void testAll(World &world)
{
	testEmptyInput(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
