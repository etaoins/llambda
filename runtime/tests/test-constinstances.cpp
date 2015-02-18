#include "binding/BooleanCell.h"
#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"

#include "core/init.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

void testAll(World &world)
{
	ASSERT_TRUE(UnitCell::isInstance(UnitCell::instance()));
	ASSERT_EQUAL(BooleanCell::trueInstance()->value(), true);
	ASSERT_EQUAL(BooleanCell::falseInstance()->value(), false);
	ASSERT_TRUE(EmptyListCell::isInstance(EmptyListCell::instance()));
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
