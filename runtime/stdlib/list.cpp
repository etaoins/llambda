#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"

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
	const BoxedDatum *datum = element;
	std::uint32_t length = 0;

	while(auto pair = datum->asBoxedPair())
	{
		length++;
		datum = pair->cdr();
	}

	return length;
}

}
