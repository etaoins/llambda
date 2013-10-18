#ifndef _LLIBY_BINDING_PROPERLIST_H
#define _LLIBY_BINDING_PROPERLIST_H

#include <iterator>

#include "BoxedListElement.h"
#include "BoxedPair.h"
#include "BoxedEmptyList.h"

namespace lliby
{

template<class T>
class ProperList
{
public:
	class ConstIterator : std::iterator<std::forward_iterator_tag, T>
	{
		friend class ProperList;
	public:
		T* operator*() const
		{
			// ProperList verifies all the cars are of type T in its constructor
			return reinterpret_cast<T*>(m_head->car());
		}

		bool operator==(const ConstIterator &other) const
		{
			return m_head == other.m_head;
		}
		
		bool operator!=(const ConstIterator &other) const
		{
			return m_head != other.m_head;
		}

		void operator++()
		{
			BoxedDatum *cdr = m_head->cdr();

			if (cdr == BoxedEmptyList::instance())
			{
				// End if the list
				m_head = nullptr;
			}
			else
			{
				// Because we're a proper list this must be a pair
				m_head = reinterpret_cast<BoxedPair*>(cdr);
			}
		}

	private:
		explicit ConstIterator(const BoxedPair *head = nullptr) :
			m_head(head)
		{
		}
		
		const BoxedPair *m_head;
	};

	explicit ProperList(const BoxedListElement *head) :
		m_head(nullptr),
		m_valid(false),
		m_length(0)
	{
		const BoxedDatum *datum = head;
		std::uint32_t length = 0;

		while(auto pair = datum_cast<BoxedPair>(datum))
		{
			length++;
			
			if (datum_cast<T>(pair->car()) == nullptr)
			{
				// Wrong element type
				return;
			}

			datum = pair->cdr();
		}

		if (datum != BoxedEmptyList::instance())
		{
			// Not a proper list
			return;
		}

		// nullptr m_head means we're an empty list
		m_head = datum_cast<BoxedPair>(head);
		m_length = length;
		m_valid = true;
	}

	bool isValid() const
	{
		return m_valid;
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
		return ConstIterator(nullptr);
	}

private:
	const BoxedPair *m_head;
	bool m_valid;
	std::uint32_t m_length;
};


}

#endif
