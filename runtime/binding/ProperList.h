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
	class ConstIterator : public std::iterator<std::forward_iterator_tag, T*>
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

		ConstIterator& operator++()
		{
			m_head = static_cast<const BoxedPair*>(m_head->cdr());
			return *this;
		}
		
		ConstIterator operator++(int postfix)
		{
			ConstIterator originalValue(*this);
			++(*this);
			return originalValue;
		}

	private:
		explicit ConstIterator(const BoxedListElement *head) :
			m_head(static_cast<const BoxedPair*>(head))
		{
		}
		
		const BoxedPair *m_head;
	};

	explicit ProperList(const BoxedListElement *head) :
		m_head(BoxedEmptyList::instance()),
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
		return ConstIterator(BoxedEmptyList::instance());
	}

private:
	const BoxedListElement *m_head;
	bool m_valid;
	std::uint32_t m_length;
};


}

#endif
