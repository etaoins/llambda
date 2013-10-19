#ifndef _LLIBY_ALLOC_RANGEALLOC_H
#define _LLIBY_ALLOC_RANGEALLOC_H

#include <iterator>
#include <assert.h>

#include "Cons.h"

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
			return m_currentCons == other.m_currentCons;
		}
		
		bool operator!=(const Iterator &other) const
		{
			return m_currentCons != other.m_currentCons;
		}

		void* operator*()
		{
			// Make sure we're dereferencing in range
			assert((m_currentCons >= m_startPointer) && (m_currentCons < m_endPointer));

			return m_currentCons;
		}
		
		Iterator& operator++()
		{
			m_currentCons++;
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
			m_currentCons--;
			return *this;
		}
		
		Iterator operator--(int postfix)
		{
			Iterator originalValue(*this);
			--(*this);
			return originalValue;
		}

	private:
		Iterator(Cons *startPointer, Cons *currentCons, Cons *endPointer) :
			m_startPointer(startPointer),
			m_currentCons(currentCons),
			m_endPointer(endPointer)
		{
		}

		Cons *m_startPointer;
		Cons *m_currentCons;
		Cons *m_endPointer;
	};

public:
	RangeAlloc(Cons *startPointer, Cons *endPointer) :
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
	Cons *m_startPointer;
	Cons *m_endPointer;
};

}
}

#endif
