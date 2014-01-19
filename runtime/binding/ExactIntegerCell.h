#ifndef _LLIBY_BINDING_EXACTINTEGERCELL_H
#define _LLIBY_BINDING_EXACTINTEGERCELL_H

#include "NumericCell.h"

namespace lliby
{

class ExactIntegerCell : public NumericCell
{
#include "generated/ExactIntegerCellMembers.h"
public:
	static ExactIntegerCell* fromValue(std::int64_t value)
	{
		return new ExactIntegerCell(value);
	}

private:
	ExactIntegerCell(std::int64_t value) :
		NumericCell(CellTypeId::ExactInteger),
		m_value(value)
	{
	}
};

}

#endif
