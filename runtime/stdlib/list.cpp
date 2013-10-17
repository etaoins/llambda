#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"
#include "core/fatal.h"

using namespace lliby;

extern "C"
{

BoxedPair *lliby_cons(BoxedDatum *car, BoxedDatum *cdr)
{
	return new BoxedPair(car, cdr);
}

BoxedDatum *lliby_car(BoxedPair *pair)
{
	return pair->car();
}

BoxedDatum *lliby_cdr(BoxedPair *pair)
{
	return pair->cdr();
}

void lliby_set_car(BoxedPair *pair, BoxedDatum *obj)
{
	return pair->setCar(obj);
}

void lliby_set_cdr(BoxedPair *pair, BoxedDatum *obj)
{
	return pair->setCdr(obj);
}

std::uint32_t lliby_length(const BoxedListElement *element) 
{
	std::int64_t length = element->listLength();;

	if (length == BoxedListElement::InvalidListLength)
	{
		_lliby_fatal("Non-list passed to list-length", element);
	}

	return length;
}

}
