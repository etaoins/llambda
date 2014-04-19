#ifndef _LLIBY_BINDING_PROPERLIST_H
#define _LLIBY_BINDING_PROPERLIST_H

#include <iterator>
#include <cassert>

#include "ListElementCell.h"
#include "PairCell.h"
#include "EmptyListCell.h"

namespace lliby
{

template<class T>
class ProperList
{
public:
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

	explicit ProperList(const ListElementCell *head) :
		m_head(EmptyListCell::instance()),
		m_valid(false),
		m_length(0)
	{
		if (auto pair = datum_cast<PairCell>(head))
		{
			// Do we have a constant list hint?
			// codegen creates these for the literal lists it generates
			// The type ID test is a bit tricky. If the list has mixed types the type ID will be CellTypeId::Invalid
			// However, DatumCell::typeIdIsTypeOrSubtype() always returns true so that works as expected
			if ((pair->listLength() != 0) && (T::typeIdIsTypeOrSubtype(pair->memberTypeId())))
			{
				// Only constants should be hinted
				// Otherwise the hints will get out of sync when the list is mutated
				assert(pair->isGlobalConstant());

				// Yup, we can skip the rest of the list
				m_head = head;
				m_valid = true;
				m_length = pair->listLength();

				return;
			}
		}

		// Manually verify the list
		const DatumCell *datum = head;
		std::uint32_t length = 0;
			
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

	std::uint32_t length() const
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
	std::uint32_t m_length;
};


}

#endif
