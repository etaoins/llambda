#include "CellRootList.h"

#include <cassert>

namespace lliby
{
namespace alloc
{

namespace
{
	void relocateNodePtr(CellRootListNode *&node, std::ptrdiff_t offset)
	{
		if (node != nullptr)
		{
			// Cast to char* so we do byte-based pointer arithmetic
			node = reinterpret_cast<CellRootListNode*>(reinterpret_cast<char*>(node) + offset);
		}
	}
}

void CellRootList::addNode(CellRootListNode &node)
{
#ifndef NDEBUG
	// Ensure this hasn't already been rooted
	for(auto existingNode = head(); existingNode != nullptr; existingNode = existingNode->next)
	{
		void *newStart = node.basePointer;
		void *newEnd = node.basePointer + node.cellCount;

		void *existingStart = existingNode->basePointer;
		void *existingEnd = existingNode->basePointer + existingNode->cellCount;

		if (((newStart >= existingStart) && (newStart < existingEnd)) ||
			((newEnd > existingStart) && (newEnd <= existingEnd)))
		{
			assert(false);
		}
	}
#endif

	// Add to the active list
	node.prev = nullptr;
	node.next = m_head;

	if (m_head)
	{
		m_head->prev = &node;
	}

	m_head = &node;
}

void CellRootList::removeNode(CellRootListNode &node)
{
	if (node.prev != nullptr)
	{
		node.prev->next = node.next;
	}
	else
	{
		// We have no previous element; we must be the head
		m_head = node.next;
	}

	if (node.next != nullptr)
	{
		node.next->prev = node.prev;
	}
}

void CellRootList::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	relocateNodePtr(m_head, offset);

	for(auto node = head(); node != nullptr; node = node->next)
	{
		relocateNodePtr(node->prev, offset);
		relocateNodePtr(node->next, offset);

		if ((node->basePointer >= oldStackStart) && (node->basePointer < oldStackEnd))
		{
			node->basePointer = reinterpret_cast<AllocCell**>(reinterpret_cast<char*>(node->basePointer) + offset);
		}
	}
}

}
}
