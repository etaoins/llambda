#ifndef _LLIBY_CORE_CONSTINSTANCES_H
#define _LLIBY_CORE_CONSTINSTANCES_H

namespace lliby
{
	class UnspecificCell;
	class BooleanCell;
	class EmptyListCell;
}

extern "C"
{

extern const lliby::UnspecificCell lliby_unspecific_value;
extern const lliby::BooleanCell lliby_false_value;
extern const lliby::BooleanCell lliby_true_value;
extern const lliby::EmptyListCell lliby_empty_list_value;

}

#endif
