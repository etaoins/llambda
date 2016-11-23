#ifndef _LLIBY_BINDING_TYPEDPAIRCELL_H
#define _LLIBY_BINDING_TYPEDPAIRCELL_H

namespace lliby
{
class World;

template<typename A, typename D>
class TypedPairCell : public PairCell
{
public:
	template<typename AV, typename DV>
	static TypedPairCell<A, D> *emplaceValues(World &world, AV carValue, DV cdrValue)
	{
		alloc::RangeAlloc allocation = alloc::allocateRange(world, 3);
		auto allocIt = allocation.begin();

		A *car = new (*allocIt++) A(carValue);
		D *cdr = new (*allocIt++) D(cdrValue);

		PairCell *pairCell = new (*allocIt++) PairCell(car, cdr);
		return static_cast<TypedPairCell<A, D>*>(pairCell);
	}

	A* car() const
	{
		return cell_unchecked_cast<A>(PairCell::car());
	}

	D* cdr() const
	{
		return cell_unchecked_cast<D>(PairCell::cdr());
	}
};


}


#endif
