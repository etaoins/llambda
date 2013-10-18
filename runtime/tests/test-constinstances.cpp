#include "binding/BoxedBoolean.h"
#include "binding/BoxedUnspecific.h"
#include "binding/BoxedEmptyList.h"

#include "core/init.h"
#include "assertions.h"

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	ASSERT_TRUE(BoxedUnspecific::isInstance(BoxedUnspecific::instance()));
	ASSERT_EQUAL(BoxedBoolean::trueInstance()->value(), true);
	ASSERT_EQUAL(BoxedBoolean::falseInstance()->value(), false);
	ASSERT_TRUE(BoxedEmptyList::isInstance(BoxedEmptyList::instance()));

	return 0;
}
