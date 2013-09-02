#include "constinstances.h"

#include "binding/UnspecificValue.h"
#include "binding/BooleanValue.h"
#include "binding/EmptyListValue.h"

extern "C"
{

// These are constant values that must be referenced through these singleton
// instances. This provides two benefits:
// 1) They can be tested for equality without deferencing their poointer.
//    Dereferencing still works as expected but this can be used as an
//    optimization 
// 2) These values can be used without an allocation and the associated stress
//    on the garbage collector
const lliby::UnspecificValue lliby_unspecific_value;
const lliby::BooleanValue lliby_false_value(false);
const lliby::BooleanValue lliby_true_value(true);
const lliby::EmptyListValue lliby_empty_list_value;

}
