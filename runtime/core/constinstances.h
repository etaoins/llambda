#ifndef _LLIBY_CORE_CONSTINSTANCES_H
#define _LLIBY_CORE_CONSTINSTANCES_H

namespace lliby
{
	class BoxedUnspecific;
	class BoxedBoolean;
	class BoxedEmptyList;
}

extern "C"
{

extern const lliby::BoxedUnspecific lliby_unspecific_value;
extern const lliby::BoxedBoolean lliby_false_value;
extern const lliby::BoxedBoolean lliby_true_value;
extern const lliby::BoxedEmptyList lliby_empty_list_value;

}

#endif
