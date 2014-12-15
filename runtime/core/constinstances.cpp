#include "constinstances.h"

#include "binding/UnitCell.h"
#include "binding/BooleanCell.h"
#include "binding/EmptyListCell.h"
#include "binding/EofObjectCell.h"

extern "C"
{

// These are constant values that must be referenced through these preconstructed instances. This provides two benefits:
// 1) They can be tested for equality without dereferencing their pointer. Dereferencing still works as expected but
//    this can be used as an optimization
// 2) These values can be used without a GC allocation and the associated stress on the garbage collector
const lliby::UnitCell llcore_unit_value;
const lliby::BooleanCell llcore_false_value(false);
const lliby::BooleanCell llcore_true_value(true);
const lliby::EmptyListCell llcore_empty_list_value;
const lliby::EofObjectCell llcore_eof_object_value;

}
