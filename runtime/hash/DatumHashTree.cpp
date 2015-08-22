#include "hash/DatumHashTree.h"
#include "binding/AnyCell.h"

#include "alloc/CellRefWalker.h"

#include <algorithm>

namespace lliby
{

namespace
{

#ifdef _LLIBY_CHECK_LEAKS
std::atomic<std::size_t> allocationCount(0);
#endif

/**
 * Returns the number of bits set to 1 in the passed bitmap
 *
 * Note that x86 only gained a POPCNT instruction in SSE 4.2. Using appropriate compiler flags can cause a large speedup
 * here.
 */
std::uint32_t bitmapPopCount(std::uint32_t bitmap)
{
	return __builtin_popcount(bitmap);
}

/**
 * The number of bits we shift our hash code at each level to calculate child node indices
 */
const std::uint32_t LevelShiftSize = 5;

/**
 * The number of bits we mask off the shifted hash code to calculate our child node indices
 */
const std::uint32_t LevelHashMask = (1 << LevelShiftSize) - 1;


template<class T, class S>
void copyWithoutIndex(T *source, S index, S oldSize, T *dest)
{
	std::copy_n(&source[0], index, &dest[0]);
	std::copy_n(&source[index + 1], oldSize - index - 1, &dest[index]);
}


class InternalNode : public DatumHashTree
{
public:
	~InternalNode()
	{
		for(std::uint32_t i = 0; i < childCount(); i++)
		{
			DatumHashTree::unref(m_children[i]);
		}
	}

	/**
	 * Creates a new internal node with a single child at the specified index
	 *
	 * @param  childIndex  Index of the new child
	 * @param  childNode   Non-empty tree to place at the specified index
	 */
	static InternalNode* fromSingleChild(std::uint32_t childIndex, DatumHashTree *childNode)
	{
		void *placement = malloc(sizeForChildCount(1));

		std::uint32_t childBitmap = 1 << childIndex;
		InternalNode *newNode = new (placement) InternalNode(childBitmap);
		newNode->m_children[0] = childNode;

		return newNode;
	}

	/**
	 * Creates a new internal node with two childiren at the specified indiciesx
	 *
	 * @param  childIndex1  Index of the first child
	 * @param  childNode1   Non-empty tree to place at childIndex1
	 * @param  childIndex2  Index of the second child
	 * @param  childNode2   Non-empty tree to place at childIndex2
	 */
	static InternalNode* fromTwoChildren(std::uint32_t childIndex1, DatumHashTree *childNode1, std::uint32_t childIndex2, DatumHashTree *childNode2)
	{
		assert(childIndex1 != childIndex2);
		void *placement = malloc(sizeForChildCount(2));

		std::uint32_t childBitmap = (1 << childIndex1) | (1 << childIndex2);

		InternalNode *newNode = new (placement) InternalNode(childBitmap);

		if (childIndex1 < childIndex2)
		{
			newNode->m_children[0] = childNode1;
			newNode->m_children[1] = childNode2;
		}
		else
		{
			newNode->m_children[0] = childNode2;
			newNode->m_children[1] = childNode1;
		}

		return newNode;
	}

	/**
	 * Returns the index for a child with a given hash code
	 *
	 * @param  level     Level of this internal node
	 * @param  hashCode  Hash code of the child
	 */
	static std::uint32_t childIndex(std::uint32_t level, std::uint32_t hashCode)
	{
		return (hashCode >> level) & LevelHashMask;
	}

	/**
	 * Returns the child at the given index or nullptr if no child exists
	 */
	DatumHashTree* childAtIndex(std::uint32_t index)
	{
		if (!hasChildAtIndex(index))
		{
			return nullptr;
		}

		DatumHashTree *child = m_children[childOffsetForIndex(index)];
		assert(child);
		return child;
	}

	/**
	 * Returns the number of children of this node
	 */
	std::uint32_t childCount() const
	{
		return bitmapPopCount(m_childBitmap);
	}

	/**
	 * Returns a new InternalNode with the non-empty child node placed at the given index
	 *
	 * @param  childIndex    Index of the new child node
	 * @param  newChildNode  Non-empty child node
	 * @param  inPlace       Indicates if the internal node can be modified in place
	 */
	InternalNode *assocChild(std::uint32_t childIndex, DatumHashTree *newChildNode, bool inPlace = false)
	{
		std::uint32_t newChildBitmap = m_childBitmap | (1 << childIndex);
		auto offset = childOffsetForIndex(childIndex);
		assert(newChildNode);

		if (newChildBitmap == m_childBitmap)
		{
			// We are staying the same size
			if (inPlace)
			{
				// We can perform this replacement in-place by replacing and unreferencing the original node.
				DatumHashTree::unref(m_children[offset]);
				m_children[offset] = newChildNode;
				ref();
				return this;
			}

			InternalNode *newInternalNode = InternalNode::createInstance(newChildBitmap);
			std::copy_n(m_children, childCount(), newInternalNode->m_children);

			for(std::uint32_t i = 0; i < childCount(); i++)
			{
				if (i == offset)
				{
					newInternalNode->m_children[i] = newChildNode;
				}
				else
				{
					DatumHashTree::ref(newInternalNode->m_children[i]);
				}
			}

			return newInternalNode;
		}
		else
		{
			// We are adding an additional child
			InternalNode *newInternalNode = InternalNode::createInstance(newChildBitmap);

			for(std::uint32_t i = 0; i < offset; i++)
			{
				newInternalNode->m_children[i] = DatumHashTree::ref(m_children[i]);
			}

			newInternalNode->m_children[offset] = newChildNode;

			for(std::uint32_t i = offset; i < childCount(); i++)
			{
				newInternalNode->m_children[i + 1] = DatumHashTree::ref(m_children[i]);
			}

			return newInternalNode;
		}

	}

	/**
	 * Returns a new InternalNode with the child node removed at the given index
	 *
	 * @param  childIndex    Index of the new child node
	 * @param  newChildNode  Non-empty child node
	 * @param  inPlace       Indicates if the internal node can be modified in place
	 */
	InternalNode *withoutChild(std::uint32_t childIndex)
	{
		std::uint32_t newChildBitmap = m_childBitmap & ~(1 << childIndex);
		assert(newChildBitmap != m_childBitmap);

		auto oldChildCount = childCount();
		auto newChildCount = oldChildCount - 1;

		auto removedOffset = childOffsetForIndex(childIndex);

		InternalNode *newInternalNode = InternalNode::createInstance(newChildBitmap);

		copyWithoutIndex(m_children, removedOffset, oldChildCount, newInternalNode->m_children);

		for(std::uint32_t i = 0; i < newChildCount; i++)
		{
			DatumHashTree::ref(newInternalNode->m_children[i]);
		}

		return newInternalNode;
	}

	/**
	 * Returns all children of this node
	 *
	 * This is a contiguous array of non-empty children of size childCount()
	 */
	DatumHashTree*const* children() const
	{
		return m_children;
	}

private:
	InternalNode(std::uint32_t childBitmap) : DatumHashTree(childBitmap)
	{
		assert(childBitmap != LeafNodeChildBitmap);
	}

	static InternalNode* createInstance(std::uint32_t childBitmap)
	{
		void *placement = malloc(sizeForBitmapIndex(childBitmap));
		return new (placement) InternalNode(childBitmap);
	}

	bool hasChildAtIndex(std::uint32_t childIndex)
	{
		return m_childBitmap & (1 << childIndex);
	}

	std::uint32_t childOffsetForIndex(std::uint32_t childIndex)
	{
		return bitmapPopCount(m_childBitmap & ((1 << childIndex) - 1));
	}

	static std::size_t sizeForBitmapIndex(std::uint32_t childBitmap)
	{
		return sizeForChildCount(bitmapPopCount(childBitmap));
	}

	static std::size_t sizeForChildCount(std::size_t childCount)
	{
		return sizeof(InternalNode) + (sizeof(DatumHashTree*) * childCount);
	}

	DatumHashTree* m_children[];
};

struct LeafNodeEntry
{
	AnyCell *key;
	AnyCell *value;
};

class LeafNode : public DatumHashTree
{
public:
	static LeafNode* createInstance(DatumHash::ResultType hashValue, std::uint32_t entryCount)
	{
		void *placement = malloc(sizeForValueCount(entryCount));
		return new (placement) LeafNode(hashValue, entryCount);
	}

	/**
	 * Returns the shared hash value of entries in this leaf node
	 */
	DatumHash::ResultType hashValue() const
	{
		return m_hashValue;
	}

	std::uint32_t entryCount() const
	{
		return m_entryCount;
	}

	LeafNodeEntry* entries()
	{
		return m_entries;
	}

	const LeafNodeEntry* entries() const
	{
		return m_entries;
	}

	/**
	 * Finds a value with the given key or nullptr if one does not exist
	 *
	 * @param  key        Key to find
	 * @return Pointer to the value or nullptr if it does not exist
	 */
	AnyCell* findLeaf(AnyCell *key)
	{
		for(std::uint32_t i = 0; i < entryCount(); i++)
		{
			LeafNodeEntry &entry = m_entries[i];

			if (key->isEqual(entry.key))
			{
				return entry.value;
			}
		}

		return nullptr;
	}

	/**
	 * Return a new leaf node with an additional value
	 *
	 * The key must have the same hash value as the leaf node
	 *
	 * @param  key      Key of the new value. If the key exists the value will be replaced.
	 * @param  value    Value to add
	 * @param  inPlace  Indicates if the leaf node can be modified in place
	 * @return LeafNode instance with the new value
	 */
	LeafNode* assocLeaf(AnyCell *key, AnyCell *value, bool inPlace = false)
	{
		// Check if the key is already in the hash
		for(std::uint32_t i = 0; i < entryCount(); i++)
		{
			LeafNodeEntry &entry = m_entries[i];

			if (key->isEqual(entry.key))
			{
				if (value == entry.value)
				{
					// This value is already in the node; avoid allocating a new node
					ref();
					return this;
				}
				else if (inPlace)
				{
					entry.value = value;

					ref();
					return this;
				}
				else
				{
					// Create a new leaf node with the value replaced
					LeafNode *newLeafNode = LeafNode::createInstance(hashValue(), m_entryCount);
					std::copy(&m_entries[0], &m_entries[m_entryCount], &newLeafNode->entries()[0]);

					newLeafNode->entries()[i].value = value;

					return newLeafNode;
				}
			}
		}

		// No existing value; we need to append
		LeafNode *newLeafNode = LeafNode::createInstance(hashValue(), m_entryCount + 1);
		std::copy(&m_entries[0], &m_entries[m_entryCount], &newLeafNode->entries()[0]);

		newLeafNode->entries()[m_entryCount] = {.key = key, .value = value};

		return newLeafNode;
	}

	/**
	 * Return a new leaf node without a key
	 *
	 * The key must have the same hash value as the leaf node
	 *
	 * @param  key      Key to remove
	 * @return LeafNode instance without the key
	 */
	DatumHashTree* withoutLeaf(AnyCell *key)
	{
		// Check if the key is already in the hash
		for(std::uint32_t i = 0; i < entryCount(); i++)
		{
			LeafNodeEntry &entry = m_entries[i];

			if (key->isEqual(entry.key))
			{
				auto newEntryCount = m_entryCount - 1;

				if (newEntryCount == 0)
				{
					// Just return an empty tree
					return nullptr;
				}

				LeafNode *newLeafNode = LeafNode::createInstance(hashValue(), newEntryCount);
				copyWithoutIndex(entries(), i, entryCount(), newLeafNode->entries());

				return newLeafNode;
			}
		}

		// No value found
		return ref();
	}


private:
	LeafNode(DatumHash::ResultType hashValue, std::uint32_t entryCount) :
		DatumHashTree(LeafNodeChildBitmap),
		m_hashValue(hashValue),
		m_entryCount(entryCount)
	{
	}

	static std::size_t sizeForValueCount(std::size_t entryCount)
	{
		return sizeof(LeafNode) + (sizeof(LeafNodeEntry) * entryCount);
	}

	DatumHash::ResultType m_hashValue;
	std::uint32_t m_entryCount;

	LeafNodeEntry m_entries[];
};

}

DatumHashTree::DatumHashTree(std::uint32_t childBitmap) :
	m_refCount(1),
	m_childBitmap(childBitmap)
{
#ifdef _LLIBY_CHECK_LEAKS
	allocationCount.fetch_add(1, std::memory_order_relaxed);
#endif
}

DatumHashTree::~DatumHashTree()
{
#ifdef _LLIBY_CHECK_LEAKS
	allocationCount.fetch_sub(1, std::memory_order_relaxed);
#endif
}

void* DatumHashTree::operator new(size_t s, void *placement)
{
	return placement;
}

void DatumHashTree::operator delete(void *value)
{
	free(value);
}

DatumHashTree* DatumHashTree::ref(DatumHashTree *tree)
{
	if (tree)
	{
		tree->ref();
	}

	return tree;
}

DatumHashTree* DatumHashTree::ref()
{
	m_refCount.fetch_add(1u, std::memory_order_relaxed);
	return this;
}

void DatumHashTree::unref(DatumHashTree *tree)
{
	if (tree != nullptr)
	{
		tree->unref();
	}
}

void DatumHashTree::unref()
{
	auto previousRefCount = m_refCount.fetch_sub(1u, std::memory_order_release);

	// Make sure we don't double unref
	assert(previousRefCount != 0);

	if (previousRefCount != 1)
	{
		// We still have references
		return;
	}

	// Make sure the memory operations from this delete are strictly after the fetch_sub
	std::atomic_thread_fence(std::memory_order_acquire);

	if (isLeafNode())
	{
		delete static_cast<LeafNode*>(this);
	}
	else
	{
		delete static_cast<InternalNode*>(this);
	}
}

std::size_t DatumHashTree::instanceCount()
{
#ifdef _LLIBY_CHECK_LEAKS
	return allocationCount;
#else
	return 0;
#endif
}

bool DatumHashTree::isLeafNode() const
{
	return m_childBitmap == LeafNodeChildBitmap;
}

DatumHashTree* DatumHashTree::fromAssocList(ProperList<PairCell> *list)
{
	DatumHashTree *tree = nullptr;
	DatumHash hasher;

	for(auto pair : *list)
	{
		auto hashValue = hasher(pair->car());
		DatumHashTree *newTree = DatumHashTree::assocAtLevel(tree, 0, pair->car(), pair->cdr(), hashValue, true);
		DatumHashTree::unref(tree);

		tree = newTree;
	}

	return tree;
}

DatumHashTree* DatumHashTree::assoc(DatumHashTree *tree, AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue)
{
	return assocAtLevel(tree, 0, key, value, hashValue);
}

AnyCell* DatumHashTree::find(DatumHashTree *tree, AnyCell *key, DatumHash::ResultType hashValue)
{
	return findAtLevel(tree, 0, key, hashValue);
}

DatumHashTree* DatumHashTree::without(DatumHashTree *tree, AnyCell *key, DatumHash::ResultType hashValue)
{
	return withoutAtLevel(tree, 0, key, hashValue);
}

std::size_t DatumHashTree::size(const DatumHashTree *tree)
{
	if (tree == nullptr)
	{
		return 0;
	}
	else if (tree->isLeafNode())
	{
		return static_cast<const LeafNode*>(tree)->entryCount();
	}
	else
	{
		std::size_t accum = 0;

		const InternalNode *internalNode = static_cast<const InternalNode*>(tree);
		for(std::uint32_t i = 0; i < internalNode->childCount(); i++)
		{
			accum += size(internalNode->children()[i]);
		}

		return accum;
	}
}

bool DatumHashTree::every(const DatumHashTree *tree, const std::function<bool(AnyCell*, AnyCell*, DatumHash::ResultType)> &pred)
{
	if (tree == nullptr)
	{
		return true;
	}
	else if (tree->isLeafNode())
	{
		auto leafNode = static_cast<const LeafNode*>(tree);

		for(std::uint32_t i = 0; i < leafNode->entryCount(); i++)
		{
			auto &entry = leafNode->entries()[i];
			if (!pred(entry.key, entry.value, leafNode->hashValue()))
			{
				return false;
			}
		}
	}
	else
	{
		auto internalNode = static_cast<const InternalNode*>(tree);

		for(std::uint32_t i = 0; i < internalNode->childCount(); i++)
		{
			if (!every(internalNode->children()[i], pred))
			{
				return false;
			}
		}
	}

	return true;
}

void DatumHashTree::walkCellRefs(DatumHashTree *tree, alloc::CellRefWalker &walker, const std::function<void(AnyCell**, AnyCell**)> &visitor)
{
	if (tree == nullptr)
	{
		return;
	}
	else if (tree->isLeafNode())
	{
		auto leafNode = static_cast<LeafNode*>(tree);

		if (!walker.shouldVisitDatumHashSubtree(tree))
		{
			return;
		}

		for(std::size_t i = 0; i < leafNode->entryCount(); i++)
		{
			auto &entry = leafNode->entries()[i];
			visitor(&entry.key, &entry.value);
		}
	}
	else
	{
		auto internalNode = static_cast<const InternalNode*>(tree);

		for(std::uint32_t i = 0; i < internalNode->childCount(); i++)
		{
			walkCellRefs(internalNode->children()[i], walker, visitor);
		}
	}
}

DatumHashTree* DatumHashTree::assocAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue, bool inPlace)
{
	if (tree == nullptr)
	{
		// Allocate a leaf node with a single value
		LeafNode *leafNode = LeafNode::createInstance(hashValue, 1);
		leafNode->entries()[0] = {.key = key, .value = value};

		return leafNode;
	}
	else if (tree->isLeafNode())
	{
		LeafNode *leafNode = static_cast<LeafNode*>(tree);

		if (leafNode->hashValue() == hashValue)
		{
			return leafNode->assocLeaf(key, value, inPlace);
		}

		auto oldChildIndex = InternalNode::childIndex(level, leafNode->hashValue());
		auto newChildIndex = InternalNode::childIndex(level, hashValue);

		if (oldChildIndex == newChildIndex)
		{
			// These children would appear at the name index in the array node. In the worst case this can happen
			// at the following levels so we can't build a final internal node directly here. Instead build an internal
			// node with the existing leaf node and call ourselves recursively.
			InternalNode *newInternalNode = InternalNode::fromSingleChild(oldChildIndex, leafNode->ref());

			DatumHashTree *mergedInternalNode = assocAtLevel(newInternalNode, level, key, value, hashValue, true);
			newInternalNode->unref();

			return mergedInternalNode;
		}
		else
		{
			// The children would appear in different indices. Build a new leaf node and directly create an internal
			// node the new leaf and existing leaf as children.
			LeafNode *newLeafNode = LeafNode::createInstance(hashValue, 1);
			newLeafNode->entries()[0] = {.key = key, .value = value};

			return InternalNode::fromTwoChildren(oldChildIndex, leafNode->ref(), newChildIndex, newLeafNode);
		}
	}
	else
	{
		InternalNode *internalNode = static_cast<InternalNode*>(tree);

		auto childIndex = InternalNode::childIndex(level, hashValue);
		DatumHashTree* childNode = internalNode->childAtIndex(childIndex);

		DatumHashTree *newChildNode = assocAtLevel(childNode, level + LevelShiftSize, key, value, hashValue, inPlace);

		if (childNode == newChildNode)
		{
			// Nothing to do!
			DatumHashTree::unref(childNode);
			return internalNode->ref();
		}
		else
		{
			// Allocate a new internal node
			return internalNode->assocChild(childIndex, newChildNode, inPlace);
		}
	}
}

AnyCell* DatumHashTree::findAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue)
{
	if (tree == nullptr)
	{
		// Empty tree
		return nullptr;
	}
	else if (tree->isLeafNode())
	{
		LeafNode *leafNode = static_cast<LeafNode*>(tree);

		if (leafNode->hashValue() != hashValue)
		{
			return nullptr;
		}

		return leafNode->findLeaf(key);
	}
	else
	{
		InternalNode *internalNode = static_cast<InternalNode*>(tree);

		DatumHashTree *childNode = internalNode->childAtIndex(InternalNode::childIndex(level, hashValue));

		if (childNode == nullptr)
		{
			return nullptr;
		}

		return findAtLevel(childNode, level + LevelShiftSize, key, hashValue);
	}
}

DatumHashTree* DatumHashTree::withoutAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue)
{
	if (tree == nullptr)
	{
		return tree;
	}
	else if (tree->isLeafNode())
	{
		LeafNode *leafNode = static_cast<LeafNode*>(tree);

		if (leafNode->hashValue() != hashValue)
		{
			// Nothing to without
			return tree->ref();
		}

		return leafNode->withoutLeaf(key);
	}
	else
	{
		InternalNode *internalNode = static_cast<InternalNode*>(tree);

		auto childIndex = InternalNode::childIndex(level, hashValue);
		DatumHashTree *childNode = internalNode->childAtIndex(childIndex);

		if (childNode == nullptr)
		{
			return tree->ref();
		}

		auto newChildNode = withoutAtLevel(childNode, level + LevelShiftSize, key, hashValue);

		if (childNode == newChildNode)
		{
			// Nothing changed
			DatumHashTree::unref(newChildNode);
			return tree->ref();
		}
		else if ((internalNode->childCount() == 1) && (newChildNode == nullptr))
		{
			// No more children - return the empty tree
			return nullptr;
		}
		else if ((internalNode->childCount() == 1) && (newChildNode->isLeafNode()))
		{
			// Single leaf node left - collapse
			return newChildNode;
		}
		else if (newChildNode)
		{
			// Note that if our remaining child is an internal node we cannot collapse it to our level. Child indices
			// are based on the node's level so moving an internal node involves rebuilding it recursively.
			return internalNode->assocChild(childIndex, newChildNode);
		}
		else
		{
			return internalNode->withoutChild(childIndex);
		}
	}
}

}
