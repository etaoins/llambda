#ifndef _LLIBY_ALLOC_ABSTRACTREFVECTOR_H
#define _LLIBY_ALLOC_ABSTRACTREFVECTOR_H

#include "alloc/AbstractRef.h"

namespace lliby
{
namespace alloc
{

/**
 * Resizable vector of GC managed values
 *
 * This has a subset of the interface of std::vector and is internally implemented using a combination of std::vector
 * and AbstractRoot. This has significantly less overhead than a std::vector of AbstractRefs
 */
template<class T>
class AbstractRefVector : public AbstractRoot<T>
{
public:
	using value_type = typename std::vector<T*>::value_type;
	using reference = typename std::vector<T*>::reference;
	using size_type = typename std::vector<T*>::size_type;
	using iterator = typename std::vector<T*>::iterator;
	using const_iterator = typename std::vector<T*>::const_iterator;
	using reverse_iterator = typename std::vector<T*>::reverse_iterator;

	iterator begin()
	{
		return m_vector.begin();
	}

	iterator end()
	{
		return m_vector.end();
	}

	reverse_iterator rbegin()
	{
		return m_vector.rbegin();
	}

	reverse_iterator rend()
	{
		return m_vector.rend();
	}

	size_type size() const
	{
		return m_vector.size();
	}

	bool empty() const
	{
		return m_vector.empty();
	}

	reference operator[](size_type n)
	{
		return m_vector[n];
	}

	void push_back(const value_type &val)
	{
		m_vector.push_back(val);
		updateRootListNode();
	}

	void resize(size_type n)
	{
		m_vector.resize(n);
		updateRootListNode();
	}

	void reserve(size_type n)
	{
		m_vector.reserve(n);
		updateRootListNode();
	}

	template<typename... Arguments>
	void insert(Arguments... parameters)
	{
		m_vector.insert(parameters...);
		updateRootListNode();
	}

	T** data()
	{
		return m_vector.data();
	}

	AbstractRefVector(const AbstractRefVector<T> &other) :
		AbstractRoot<T>(AbstractRoot<T>::m_rootList, nullptr, 0),
		m_vector(other.m_vector)
	{
		updateRootListNode();
	}

	AbstractRefVector(AbstractRefVector<T> &&other) :
		AbstractRoot<T>(AbstractRoot<T>::m_rootList, nullptr, 0),
		m_vector(std::move(other.m_vector))
	{
		updateRootListNode();
	}

protected:
	template<typename... Arguments>
	explicit AbstractRefVector(CellRootList *rootList, Arguments... parameters) :
		AbstractRoot<T>(rootList, nullptr, 0),
		m_vector(parameters...)
	{
		updateRootListNode();
	}

private:
	void updateRootListNode()
	{
		AbstractRoot<T>::m_node.basePointer = reinterpret_cast<AllocCell**>(m_vector.data());
		AbstractRoot<T>::m_node.cellCount = m_vector.size();
	}

	std::vector<T*> m_vector;
};

}
}

#endif
