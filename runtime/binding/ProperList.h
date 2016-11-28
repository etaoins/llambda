#ifndef _LLIBY_BINDING_PROPERLIST_H
#define _LLIBY_BINDING_PROPERLIST_H

#include "ListElementCell.h"
#include "EmptyListCell.h"
#include "PairCell.h"

#include "alloc/RangeAlloc.h"
#include "alloc/StrongRef.h"

#include <iterator>

namespace lliby
{
	/**
	 * Represents the head of a proper list
	 *
	 * Proper lists are defined by Scheme to be a pair with a cdr of either a the empty list or another proper list.
	 * On the Scheme side they're defined with the type (Listof \<type\>) where \<type\> is the type of the car values.
	 * This is the analogous C++ representation. It supports forward iteration, size calculations and construction of
	 * new instances via ProperList::create().
	 *
	 * This implements ProperList::isInstance() which allows cell_cast<> and cell_unchecked_cast<> to convert other
	 * cells to the appropriate ProperList type.
	 */
	template<class T>
	class ProperList : public ListElementCell
	{
	public:
		using size_type = std::uint32_t;

		class Iterator : public std::iterator<std::forward_iterator_tag, T*>
		{
			friend class ProperList;
		public:
			T* operator*() const
			{
				auto pairHead = cell_unchecked_cast<const PairCell>(m_head);
				return cell_unchecked_cast<T>(pairHead->car());
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
				auto pairHead = cell_unchecked_cast<const PairCell>(m_head);
				m_head = cell_unchecked_cast<const ListElementCell>(pairHead->cdr());

				return *this;
			}

			Iterator operator++(int postfix)
			{
				Iterator originalValue(*this);
				++(*this);
				return originalValue;
			}

		private:
			explicit Iterator(const ListElementCell *head) :
				m_head(head)
			{
			}

			const ListElementCell *m_head;
		};

		/**
		 * Returns an iterator pointing to the beginning of this proper list
		 */
		typename ProperList<T>::Iterator begin() const
		{
			return typename ProperList<T>::Iterator(this);
		}

		typename ProperList<T>::Iterator end() const
		{
			return typename ProperList<T>::Iterator(EmptyListCell::instance());
		}

		/**
		 * Returns true if this proper list is empty
		 *
		 * This is more efficient than size() == 0
		 */
		bool empty() const
		{
			return begin() == end();
		}

		/**
		 * Returns this size of this proper list
		 *
		 * If we have a length hint then this is an O(1) operation. Otherwise it's O(n) with the length  of the list.
		 * For that reason the length should be cached whenever possible.
		 */
		size_type size() const
		{
			// Try a length hint first
			if (auto pair = cell_cast<PairCell>(this))
			{
				if (pair->listLength() != 0)
				{
					return pair->listLength();
				}
			}

			// Calculate it manually
			return std::distance(begin(), end());
		}

		/**
		 * Creates a new ProperList instance containing the passed elements
		 */
		static ProperList<T> *create(World &world, alloc::StrongRefVector<T> &elements)
		{
			return createFromGcRooted(world, elements);
		}

		static ProperList<T> *create(World &world, std::vector<T*> &elements)
		{
			alloc::StrongRoot<T> gcRoot(world, elements.data(), elements.size());
			return createFromGcRooted(world, elements);
		}

		static ProperList<T> *create(World &world, std::initializer_list<T*> elementsList)
		{
			alloc::StrongRefVector<T> elements(world, elementsList.begin(), elementsList.end());
			return create(world, elements);
		}

		/**
		 * Creates a new ProperList instance by passing each value to our member type's constructor
		 *
		 * This is useful for creating lists of boxed values from containers of unboxsed values - for example, a list of
		 * ExactIntegerCell from a std::vector<std::int64_t>. In addition to being more convenient than constructing
		 * each value it also has significantly less GC overhead due to allocating the list's values and pairs at once.
		 */
		template<typename Container>
		static ProperList<T> *emplaceValues(World &world, const Container &values)
		{
			if (values.size() == 0)
			{
				return EmptyListCell::asProperList<T>();
			}

			alloc::RangeAlloc allocation = alloc::allocateRange(world, values.size() * 2);

			auto allocIt = allocation.begin();
			auto valueIt = values.begin();
			for(auto left = values.size(); left; left--)
			{
				void *pairCell = *allocIt++;
				void *valueCell = *allocIt++;
				auto cdr = (left == 1) ? EmptyListCell::instance() : static_cast<AnyCell*>(*allocIt);

				new (pairCell) PairCell(new (valueCell) T(*valueIt++), cdr, left);
			}

			return static_cast<ProperList<T>*>(*allocation.begin());
		}

		template<typename V>
		static ProperList<T> *emplaceValues(World &world, const std::initializer_list<V> &valuesList)
		{
			return emplaceValues<std::initializer_list<V>>(world, valuesList);
		}

		/**
		 * Returns true if the passed cell is a proper list of the correct type
		 *
		 * This is used to implement cell_cast<> for ProperList instances.
		 */
		static bool isInstance(const AnyCell *cell)
		{
			while(auto pair = cell_cast<PairCell>(cell))
			{
				if (!T::isInstance(pair->car()))
				{
					return false;
				}

				cell = pair->cdr();
			}

			if (cell != EmptyListCell::instance())
			{
				return false;
			}

			return true;
		}

	private:
		template<class Container>
		static ProperList<T> *createFromGcRooted(World &world, Container &elements)
		{
			if (elements.empty())
			{
				return EmptyListCell::asProperList<T>();
			}

			alloc::RangeAlloc allocation = alloc::allocateRange(world, elements.size());
			auto allocIt = allocation.end();

			auto it = elements.rbegin();
			AnyCell *cdr = EmptyListCell::instance();
			std::uint32_t tailSize = 0;

			for(;it != elements.rend(); it++)
			{
				cdr = new (*--allocIt) PairCell(*it, cdr, ++tailSize);
			}

			return static_cast<ProperList<T>*>(cdr);
		}

	};

	// This aliases is for code documentation purposes. They use ProperLists for their representation in the ABI
	template<class T> using RestValues = ProperList<T>;
}

#endif
