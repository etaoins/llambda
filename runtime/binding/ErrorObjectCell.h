#ifndef _LLIBY_BINDING_ERROROBJECTCELL_H
#define _LLIBY_BINDING_ERROROBJECTCELL_H

#include "DatumCell.h"

namespace lliby
{

class StringCell;
class ListElementCell;

class ErrorObjectCell : public DatumCell
{
#include "generated/ErrorObjectCellMembers.h"
public:
	static ErrorObjectCell *createInstance(StringCell *message, ListElementCell *irritants);
	
	// These are used by the garbage collector to update the datum pointers during compaction
	StringCell** messageRef()
	{
		return &m_message;
	}

	ListElementCell** irritantsRef()
	{
		return &m_irritants;
	}
	
private:
	ErrorObjectCell(StringCell *message, ListElementCell *irritants) :
		DatumCell(CellTypeId::ErrorObject),
		m_message(message),
		m_irritants(irritants)
	{
	}
};

}

#endif
