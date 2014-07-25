#ifndef _LLIBY_BINDING_PROPERLIST_H
#define _LLIBY_BINDING_PROPERLIST_H

#include <iterator>
#include <cassert>

#include "ListElementCell.h"
#include "PairCell.h"
#include "EmptyListCell.h"
#include "RestArgument.h"

namespace lliby
{

template<class T>
class ProperList
{
public:
	typedef std::uint32_t size_type;

	class ConstIterator : public std::iterator<std::forward_iterator_tag, T*>
	{
		friend class ProperList;
	public:
		T* operator*() const
		{
			// ProperList verifies all the cars are of type T in its constructor
			auto pairHead = datum_unchecked_cast<const PairCell>(m_head);
			return datum_unchecked_cast<T>(pairHead->car());
		}

		bool operator==(const ConstIterator &other) const
		{
			return m_head == other.m_head;
		}
		
		bool operator!=(const ConstIterator &other) const
		{
			return m_head != other.m_head;
		}

		ConstIterator& operator++()
		{
			auto pairHead = datum_unchecked_cast<const PairCell>(m_head);
			m_head = datum_unchecked_cast<const ListElementCell>(pairHead->cdr());

			return *this;
		}
		
		ConstIterator operator++(int postfix)
		{
			ConstIterator originalValue(*this);
			++(*this);
			return originalValue;
		}

	private:
		explicit ConstIterator(const ListElementCell *head) :
			m_head(head)
		{
		}
		
		const ListElementCell *m_head;
	};
	
	
	explicit ProperList(const RestArgument<T> *head) :
		m_head(head),
		m_valid(true),
		m_length(0)
	{
		// This list has already been verified by Scheme; we just need to find its length
		const DatumCell *datum = head;
			
		while(auto pair = datum_cast<PairCell>(datum))
		{
			if (pair->listLength() != 0)
			{
				// We have a list length hint
				m_length += pair->listLength();
				break;
			}

			// No length hint, keep checking 
			datum = pair->cdr();
			m_length++;
		}
	}

	explicit ProperList(const ListElementCell *head) :
		m_head(EmptyListCell::instance()),
		m_valid(false),
		m_length(0)
	{
		// Manually verify the list
		const DatumCell *datum = head;
		size_type length = 0;
			
		while(auto pair = datum_cast<PairCell>(datum))
		{
			length++;
			
			if (datum_cast<T>(pair->car()) == nullptr)
			{
				// Wrong element type
				return;
			}

			datum = pair->cdr();
		}

		if (datum != EmptyListCell::instance())
		{
			// Not a proper list
			return;
		}

		m_head = head;
		m_valid = true;
		m_length = length;
	}

	bool isValid() const
	{
		return m_valid;
	}

	bool isEmpty() const
	{
		return m_length == 0;
	}

	size_type length() const
	{
		return m_length;
	}

	ConstIterator begin() const
	{
		return ConstIterator(m_head);
	}

	ConstIterator end() const
	{
		return ConstIterator(EmptyListCell::instance());
	}

private:
	const ListElementCell *m_head;
	bool m_valid;
	size_type m_length;
};


}

#endif
