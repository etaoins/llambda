#ifndef _LLIBY_BINDING_ERROROBJECTCELL_H
#define _LLIBY_BINDING_ERROROBJECTCELL_H

#include "AnyCell.h"

namespace lliby
{

class World;
class StringCell;
class ListElementCell;

class ErrorObjectCell : public AnyCell
{
#include "generated/ErrorObjectCellMembers.h"
public:
	ErrorObjectCell(StringCell *message, ListElementCell *irritants) :
		AnyCell(CellTypeId::ErrorObject),
		m_message(message),
		m_irritants(irritants)
	{
	}

	static ErrorObjectCell *createInstance(World &world, StringCell *message, ListElementCell *irritants);
	
	// These are used by the garbage collector to update the cell pointers during compaction
	StringCell** messageRef()
	{
		return &m_message;
	}

	ListElementCell** irritantsRef()
	{
		return &m_irritants;
	}
};

}

#endif
