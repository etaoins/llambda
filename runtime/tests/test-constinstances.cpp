#include "binding/BooleanValue.h"
#include "binding/UnspecificValue.h"
#include "binding/EmptyListValue.h"

#include "core/init.h"
#include "assertions.h"

int main(int argc, char *argv[])
{
	using namespace lliby;

	lliby_init();

	ASSERT_TRUE(UnspecificValue::instance()->asUnspecificValue() != nullptr);
	ASSERT_EQUAL(BooleanValue::trueInstance()->value(), true);
	ASSERT_EQUAL(BooleanValue::falseInstance()->value(), false);
	ASSERT_TRUE(EmptyListValue::instance()->asEmptyListValue() != nullptr);

	return 0;
}
