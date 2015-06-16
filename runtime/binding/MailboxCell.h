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

	/**
	 * Returns the actor::Mailbox this cell is pointing to
	 *
	 * Note that Mailbox cells are weak references. This function attempts to lock a strong reference but it may fail.
	 * Callers should ensure a reference was obtained.
	 */
	std::shared_ptr<actor::Mailbox> lockedMailbox() const
	{
		return m_mailbox.lock();
	}

	/**
	 * Returns a weak reference to the the Mailbox
	 */
	const std::weak_ptr<actor::Mailbox>& mailboxRef() const
	{
		return m_mailbox;
	}

	static MailboxCell* createInstance(World &world, const std::weak_ptr<actor::Mailbox> &mailbox);

	void finalizeMailbox();
};

}


#endif
