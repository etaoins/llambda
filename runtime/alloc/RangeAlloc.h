#ifndef _LLIBY_ALLOC_RANGEALLOC_H
#define _LLIBY_ALLOC_RANGEALLOC_H

#include <iterator>
#include <assert.h>

#include "alloc/AllocCell.h"

namespace lliby
{
namespace alloc
{

class RangeAlloc
{
public:
	class Iterator : public std::iterator<std::bidirectional_iterator_tag, void *>
	{
		friend class RangeAlloc;
	public:
		bool operator==(const Iterator &other) const
		{
			return m_currentCell == other.m_currentCell;
		}

		bool operator!=(const Iterator &other) const
		{
			return m_currentCell != other.m_currentCell;
		}

		void* operator*()
		{
#ifndef NDEBUG
			// Make sure we're dereferencing in range
			assert((m_currentCell >= m_startPointer) && (m_currentCell < m_endPointer));
#endif

			return m_currentCell;
		}

		Iterator& operator++()
		{
			m_currentCell++;
			return *this;
		}

		Iterator operator++(int postfix)
		{
			Iterator originalValue(*this);
			++(*this);
			return originalValue;
		}

		Iterator& operator--()
		{
			m_currentCell--;
			return *this;
		}

		Iterator operator--(int postfix)
		{
			Iterator originalValue(*this);
			--(*this);
			return originalValue;
		}

	private:
		Iterator(AllocCell *startPointer, AllocCell *currentCell, AllocCell *endPointer) :
#ifndef NDEBUG
			m_startPointer(startPointer),
			m_endPointer(endPointer),
#endif
			m_currentCell(currentCell)
		{
		}

#ifndef NDEBUG
		AllocCell *m_startPointer;
		AllocCell *m_endPointer;
#endif

		AllocCell *m_currentCell;
	};

public:
	RangeAlloc(AllocCell *startPointer, AllocCell *endPointer) :
		m_startPointer(startPointer),
		m_endPointer(endPointer)
	{
	}

	std::size_t size() const
	{
		return m_endPointer - m_startPointer;
	}

	Iterator begin()
	{
		return Iterator(m_startPointer, m_startPointer, m_endPointer);
	}

	Iterator end()
	{
		return Iterator(m_startPointer, m_endPointer, m_endPointer);
	}

private:
	AllocCell *m_startPointer;
	AllocCell *m_endPointer;
};

}
}

#endif
