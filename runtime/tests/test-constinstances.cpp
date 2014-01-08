#include "binding/BooleanCell.h"
#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"

#include "core/init.h"
#include "assertions.h"

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	ASSERT_TRUE(UnitCell::isInstance(UnitCell::instance()));
	ASSERT_EQUAL(BooleanCell::trueInstance()->value(), true);
	ASSERT_EQUAL(BooleanCell::falseInstance()->value(), false);
	ASSERT_TRUE(EmptyListCell::isInstance(EmptyListCell::instance()));

	return 0;
}
