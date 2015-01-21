#include "CellRootList.h"

#include <cassert>

namespace lliby
{
namespace alloc
{

namespace
{
	template<class T>
	void relocatePtr(T *&ptr, std::ptrdiff_t offset)
	{
		if (ptr != nullptr)
		{
			// Cast to char* so we do byte-based pointer arithmetic
			ptr = reinterpret_cast<T*>(reinterpret_cast<char*>(ptr) + offset);
		}
	}
}

void CellRootIterable::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	relocatePtr(m_next, offset);
}

void CellRootListNode::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	CellRootIterable::relocate(offset, oldStackStart, oldStackEnd);

	relocatePtr(m_prev, offset);
}

void ExternalRootListNode::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	CellRootListNode::relocate(offset, oldStackStart, oldStackEnd);

	if ((m_basePointer >= oldStackStart) && (m_basePointer < oldStackEnd))
	{
		relocatePtr(m_basePointer, offset);
	}
}

void InternalRootListNode::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	// We don't need any special handling
	CellRootListNode::relocate(offset, oldStackStart, oldStackEnd);
}

void CellRootList::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	CellRootIterable::relocate(offset, oldStackStart, oldStackEnd);

	for(auto node = head(); node != nullptr; node = node->next())
	{
		// This could be a virtual function but that would bloat every node with a vtable pointer
		// Use simple conditional dispatch instead
		if (node->isInternal())
		{
			static_cast<InternalRootListNode*>(node)->relocate(offset, oldStackStart, oldStackEnd);
		}
		else
		{
			static_cast<ExternalRootListNode*>(node)->relocate(offset, oldStackStart, oldStackEnd);
		}
	}
}

}
}
