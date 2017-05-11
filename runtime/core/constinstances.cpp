#include "constinstances.h"

#include "binding/UnitCell.h"
#include "binding/BooleanCell.h"
#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"
#include "binding/IntegerCell.h"
#include "alloc/GarbageState.h"

extern "C"
{

using namespace lliby;

// These are constant values that must be referenced through these preconstructed instances. This provides two benefits:
// 1) They can be tested for equality without dereferencing their pointer. Dereferencing still works as expected but
//    this can be used as an optimisation
// 2) These values can be used without a GC allocation and the associated stress on the garbage collector
const UnitCell llcore_unit_value;
const BooleanCell llcore_false_value;
const BooleanCell llcore_true_value;
const EmptyListCell llcore_empty_list_value;
const EofObjectCell llcore_eof_object_value;

const IntegerCell llcore_small_integer_values[SmallIntegerCount] = {
	IntegerCell(0, GarbageState::GlobalConstant),
	IntegerCell(1, GarbageState::GlobalConstant),
	IntegerCell(2, GarbageState::GlobalConstant),
	IntegerCell(3, GarbageState::GlobalConstant),
	IntegerCell(4, GarbageState::GlobalConstant),
	IntegerCell(5, GarbageState::GlobalConstant),
	IntegerCell(6, GarbageState::GlobalConstant),
	IntegerCell(7, GarbageState::GlobalConstant),
	IntegerCell(8, GarbageState::GlobalConstant),
	IntegerCell(9, GarbageState::GlobalConstant),
	IntegerCell(10, GarbageState::GlobalConstant),
	IntegerCell(11, GarbageState::GlobalConstant),
	IntegerCell(12, GarbageState::GlobalConstant),
	IntegerCell(13, GarbageState::GlobalConstant),
	IntegerCell(14, GarbageState::GlobalConstant),
	IntegerCell(15, GarbageState::GlobalConstant),
};

}
