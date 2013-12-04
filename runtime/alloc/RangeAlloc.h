#ifndef _LLIBY_ALLOC_RANGEALLOC_H
#define _LLIBY_ALLOC_RANGEALLOC_H

#include <iterator>
#include <assert.h>

#include "Cell.h"

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
			// Make sure we're dereferencing in range
			assert((m_currentCell >= m_startPointer) && (m_currentCell < m_endPointer));

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
		Iterator(Cell *startPointer, Cell *currentCell, Cell *endPointer) :
			m_startPointer(startPointer),
			m_currentCell(currentCell),
			m_endPointer(endPointer)
		{
		}

		Cell *m_startPointer;
		Cell *m_currentCell;
		Cell *m_endPointer;
	};

public:
	RangeAlloc(Cell *startPointer, Cell *endPointer) :
		m_startPointer(startPointer),
		m_endPointer(endPointer)
	{
	}
	
	size_t size() const
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
	Cell *m_startPointer;
	Cell *m_endPointer;
};

}
}

#endif
