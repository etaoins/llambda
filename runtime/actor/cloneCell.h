#ifndef _LLIBY_ACTOR_CLONECELL_H
#define _LLIBY_ACTOR_CLONECELL_H

namespace lliby
{
class AnyCell;
class World;

namespace alloc
{
class Heap;
}

namespace actor
{

/**
 * Thrown by cloneCell() when a cell or one of its child cells cannot be cloned
 *
 * The destination heap will be in an undefined but self-consistent state. This means it is safe to garbage collect
 * or finalise the heap.
 */
class UnclonableCellException
{
public:
	UnclonableCellException(AnyCell *cell, const char *message) :
		m_cell(cell),
		m_message(message)
	{
	}

	/**
	 * Returns the unclonable cell
	 */
	AnyCell *cell() const
	{
		return m_cell;
	}

	/**
	 * Returns a user-readable string describing the reason for cloning failure
	 */
	const char *message() const
	{
		return m_message;
	}

	/**
	 * Converts this exception to a Scheme error
	 *
	 * @param  world     World to signal the error in
	 * @param  obj       Object that was the source of the UTF-8 encoded data
	 */
	[[noreturn]]
	void signalSchemeError(World &world, const char *procName);

private:
	AnyCell *m_cell;
	const char *m_message;
};

/**
 * Creates a functionally equivalent copy of the passed cell in the passed heap
 *
 * Certain cells cannot be cloned. An UnclonableCellException will be thrown in that case.
 */
AnyCell *cloneCell(alloc::Heap &heap, AnyCell *cell);

}
}

#endif
