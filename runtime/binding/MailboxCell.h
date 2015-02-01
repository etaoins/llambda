#ifndef _LLIBY_BINDING_MAILBOXCELL_H
#define _LLIBY_BINDING_MAILBOXCELL_H

#include "AnyCell.h"

#include <memory>

namespace lliby
{
class World;

namespace actor
{
class Mailbox;
}

class MailboxCell : public AnyCell
{
#include "generated/MailboxCellMembers.h"
public:
	explicit MailboxCell(const std::weak_ptr<actor::Mailbox> &mailbox) :
		AnyCell(CellTypeId::Mailbox),
		m_mailbox(mailbox)
	{
	}

	static MailboxCell* createInstance(World &world, const std::weak_ptr<actor::Mailbox> &mailbox);

	void finalizeMailbox();
};

}


#endif
