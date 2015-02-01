#include "MailboxCell.h"

#include "alloc/allocator.h"

namespace lliby
{

MailboxCell* MailboxCell::createInstance(World &world, const std::weak_ptr<actor::Mailbox> &mailbox)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) MailboxCell(mailbox);
}

void MailboxCell::finalizeMailbox()
{
	m_mailbox.~weak_ptr();
}

}
