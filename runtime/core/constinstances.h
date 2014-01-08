#ifndef _LLIBY_CORE_CONSTINSTANCES_H
#define _LLIBY_CORE_CONSTINSTANCES_H

namespace lliby
{
	class UnitCell;
	class BooleanCell;
	class EmptyListCell;
}

extern "C"
{

extern const lliby::UnitCell lliby_unit_value;
extern const lliby::BooleanCell lliby_false_value;
extern const lliby::BooleanCell lliby_true_value;
extern const lliby::EmptyListCell lliby_empty_list_value;

}

#endif
