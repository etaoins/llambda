#include "constinstances.h"

#include "binding/UnspecificCell.h"
#include "binding/BooleanCell.h"
#include "binding/EmptyListCell.h"

extern "C"
{

// These are constant values that must be referenced through these singleton
// instances. This provides two benefits:
// 1) They can be tested for equality without deferencing their poointer.
//    Dereferencing still works as expected but this can be used as an
//    optimization 
// 2) These values can be used without an allocation and the associated stress
//    on the garbage collector
const lliby::UnspecificCell lliby_unspecific_value;
const lliby::BooleanCell lliby_false_value(false);
const lliby::BooleanCell lliby_true_value(true);
const lliby::EmptyListCell lliby_empty_list_value;

}
