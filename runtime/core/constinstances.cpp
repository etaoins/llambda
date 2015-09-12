#include "constinstances.h"

#include "binding/UnitCell.h"
#include "binding/BooleanCell.h"
#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"
#include "binding/ExactIntegerCell.h"
#include "alloc/GarbageState.h"

extern "C"
{

using namespace lliby;

// These are constant values that must be referenced through these preconstructed instances. This provides two benefits:
// 1) They can be tested for equality without dereferencing their pointer. Dereferencing still works as expected but
//    this can be used as an optimisation
// 2) These values can be used without a GC allocation and the associated stress on the garbage collector
const UnitCell llcore_unit_value;
const BooleanCell llcore_false_value(false);
const BooleanCell llcore_true_value(true);
const EmptyListCell llcore_empty_list_value;
const EofObjectCell llcore_eof_object_value;

const ExactIntegerCell llcore_small_exact_integer_values[SmallExactIntegerCount] = {
	ExactIntegerCell(0, GarbageState::GlobalConstant),
	ExactIntegerCell(1, GarbageState::GlobalConstant),
	ExactIntegerCell(2, GarbageState::GlobalConstant),
	ExactIntegerCell(3, GarbageState::GlobalConstant),
	ExactIntegerCell(4, GarbageState::GlobalConstant),
	ExactIntegerCell(5, GarbageState::GlobalConstant),
	ExactIntegerCell(6, GarbageState::GlobalConstant),
	ExactIntegerCell(7, GarbageState::GlobalConstant),
	ExactIntegerCell(8, GarbageState::GlobalConstant),
	ExactIntegerCell(9, GarbageState::GlobalConstant),
	ExactIntegerCell(10, GarbageState::GlobalConstant),
	ExactIntegerCell(11, GarbageState::GlobalConstant),
	ExactIntegerCell(12, GarbageState::GlobalConstant),
	ExactIntegerCell(13, GarbageState::GlobalConstant),
	ExactIntegerCell(14, GarbageState::GlobalConstant),
	ExactIntegerCell(15, GarbageState::GlobalConstant),
};

}
