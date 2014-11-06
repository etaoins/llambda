#ifndef _LLIBY_BINDING_ERROROBJECTCELL_H
#define _LLIBY_BINDING_ERROROBJECTCELL_H

#include "AnyCell.h"
#include "ProperList.h"
#include "ErrorCategory.h"

namespace lliby
{

class World;
class StringCell;

class ErrorObjectCell : public AnyCell
{
#include "generated/ErrorObjectCellMembers.h"
public:
	ErrorObjectCell(StringCell *message, ProperList<AnyCell> *irritants, ErrorCategory category = ErrorCategory::Default) :
		AnyCell(CellTypeId::ErrorObject),
		m_category(category),
		m_message(message),
		m_irritants(irritants)
	{
	}

	static ErrorObjectCell *createInstance(World &world, StringCell *message, ProperList<AnyCell> *irritants, ErrorCategory category = ErrorCategory::Default);

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
