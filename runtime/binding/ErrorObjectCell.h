#ifndef _LLIBY_BINDING_ERROROBJECTCELL_H
#define _LLIBY_BINDING_ERROROBJECTCELL_H

#include "AnyCell.h"
#include "ProperList.h"

namespace lliby
{

class World;
class StringCell;

class ErrorObjectCell : public AnyCell
{
#include "generated/ErrorObjectCellMembers.h"
public:
	ErrorObjectCell(StringCell *message, ProperList<AnyCell> *irritants) :
		AnyCell(CellTypeId::ErrorObject),
		m_message(message),
		m_irritants(irritants)
	{
	}

	static ErrorObjectCell *createInstance(World &world, StringCell *message, ProperList<AnyCell> *irritants);

	// These are used by the garbage collector to update the cell pointers during compaction
	StringCell** messageRef()
	{
		return &m_message;
	}

	ProperList<AnyCell>** irritantsRef()
	{
		return &m_irritants;
	}
};

}

#endif
