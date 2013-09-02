#ifndef _LLIBY_CORE_CONSTINSTANCES_H
#define _LLIBY_CORE_CONSTINSTANCES_H

namespace lliby
{
	class UnspecificValue;
	class BooleanValue;
	class EmptyListValue;
}

extern "C"
{

extern const lliby::UnspecificValue lliby_unspecific_value;
extern const lliby::BooleanValue lliby_false_value;
extern const lliby::BooleanValue lliby_true_value;
extern const lliby::EmptyListValue lliby_empty_list_value;

}

#endif
