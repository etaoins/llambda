#ifndef _LLIBY_BINDING_CHARRANGE_H
#define _LLIBY_BINDING_CHARRANGE_H

#include "unicode/utf8.h"

namespace lliby
{

class CharRange
{
public:
	using size_type = std::uint32_t;

	class Iterator : public std::iterator<std::forward_iterator_tag, UnicodeChar>
	{
		friend class CharRange;
	public:
		/**
		 * Returns the current value and increments the iterator
		 *
		 * This is not an STL-style iterator method. However, LLVM has trouble optimising *it++ in to this.
		 */
		UnicodeChar next()
		{
			return utf8::decodeChar(&m_head);
		}

		UnicodeChar operator*() const
		{
			// We do not want to increment our head here; create a copy
			const std::uint8_t *head(m_head);
			return utf8::decodeChar(&head);
		}

		bool operator==(const Iterator &other) const
		{
			return m_head == other.m_head;
		}

		bool operator!=(const Iterator &other) const
		{
			return m_head != other.m_head;
		}

		Iterator& operator++()
		{
			m_head += utf8::bytesInSequence(*m_head);
			return *this;
		}

		Iterator operator++(int postfix)
		{
			Iterator originalValue(*this);
			++(*this);
			return originalValue;
		}

	private:
		explicit Iterator(const std::uint8_t *head) :
			m_head(head)
		{
		}

		const std::uint8_t *m_head;
	};

	CharRange() :
		m_beginPointer(nullptr),
		m_endPointer(nullptr),
		m_charCount(0)
	{
	}

	CharRange(const std::uint8_t *beginPointer, const std::uint8_t *endPointer, size_type charCount) :
		m_beginPointer(beginPointer),
		m_endPointer(endPointer),
		m_charCount(charCount)
	{
	}

	bool valid() const
	{
		return m_beginPointer != nullptr;
	};

	size_type size() const
	{
		return m_charCount;
	}

	Iterator begin() const
	{
		return Iterator(m_beginPointer);
	}

	Iterator end() const
	{
		return Iterator(m_endPointer);
	}

	unsigned int byteSize() const
	{
		return m_endPointer - m_beginPointer;
	}

	const std::uint8_t *byteBegin() const
	{
		return m_beginPointer;
	}

	const std::uint8_t *byteEnd() const
	{
		return m_endPointer;
	}

private:
	const std::uint8_t *m_beginPointer;
	const std::uint8_t *m_endPointer;
	size_type m_charCount;
};

}

#endif
