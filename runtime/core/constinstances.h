#ifndef _LLIBY_CORE_CONSTINSTANCES_H
#define _LLIBY_CORE_CONSTINSTANCES_H

#include <cstddef>

namespace lliby
{
	class UnitCell;
	class BooleanCell;
	class EmptyListCell;
	class EofObjectCell;
	class ExactIntegerCell;
}

extern "C"
{

static const std::size_t SmallExactIntegerCount = 16;

extern const lliby::UnitCell llcore_unit_value;
extern const lliby::BooleanCell llcore_false_value;
extern const lliby::BooleanCell llcore_true_value;
extern const lliby::EmptyListCell llcore_empty_list_value;
extern const lliby::EofObjectCell llcore_eof_object_value;
extern const lliby::ExactIntegerCell llcore_small_exact_integer_values[SmallExactIntegerCount];

}

#endif
