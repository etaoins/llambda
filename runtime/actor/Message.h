#ifndef _LLIBY_ACTOR_MESSAGE_H
#define _LLIBY_ACTOR_MESSAGE_H

#include "binding/AnyCell.h"
#include "alloc/Heap.h"

#include <memory>

namespace lliby
{
namespace actor
{

class Mailbox;

/**
 * Message for a Mailbox
 *
 * This contains a messageCell and a heap. The messageCell is the message actually being sent. If the message cell is
 * not a constant a non-empty heap will also be passed containing the message cell and its children.
 */
class Message
{
public:
	enum class Type
	{
		/**
		 * User message sent from (tell) or (ask) including poison pills
		 */
		User,

		/**
		 * Message generated from supervised actor failure
		 */
		SupervisedFailure
	};

	/**
	 * Creates a new message from the passed cell
	 *
	 * This clones the message cell in to a new heap and returns the resulting message. If an unclonable cell is
	 * encountered then an UnclonableCellException will be thrown.
	 *
	 * @param  cell    Cell to copy in to the message
	 * @param  sender  Mailbox of the sender
	 * @param  type    Type of the message
	 */
	static Message *createFromCell(AnyCell *cell, const std::shared_ptr<Mailbox> &sender, Type = Type::User);

	/**
	 * Returns the type of the message
	 */
	Type type() const
	{
		return m_type;
	}

	AnyCell* messageCell() const
	{
		return m_messageCell;
	}

	alloc::Heap& heap()
	{
		return m_heap;
	}

	const std::weak_ptr<Mailbox>& sender() const
	{
		return m_sender;
	}

private:
	static const std::size_t InitialHeapSegmentSize = 128;

	Message() :
		m_heap(InitialHeapSegmentSize)
	{
	}

	Type m_type;

	AnyCell *m_messageCell;
	alloc::Heap m_heap;

	std::weak_ptr<Mailbox> m_sender;
};

}
}

#endif
