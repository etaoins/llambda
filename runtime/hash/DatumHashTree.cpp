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

std::uint32_t bitmapPopCount(std::uint32_t bitmap)
{
	return __builtin_popcount(bitmap);
}

const std::uint32_t LevelShiftSize = 5;
const std::uint32_t LevelHashMask = (1 << LevelShiftSize) - 1;
const std::uint32_t LevelNodeCount = 1 << LevelShiftSize;

class ArrayNode : public DatumHashTree
{
public:
	~ArrayNode()
	{
		for(std::uint32_t i = 0; i < childCount(); i++)
		{
			DatumHashTree::unref(m_children[i]);
		}
	}

	static ArrayNode* createInstance(std::uint32_t bitmapIndex)
	{
		void *placement = malloc(sizeForBitmapIndex(bitmapIndex));
		return new (placement) ArrayNode(bitmapIndex);
	}

	static ArrayNode* fromSingleChild(std::uint32_t childIndex, DatumHashTree *newChildNode)
	{
		void *placement = malloc(sizeForChildCount(1));

		std::uint32_t bitmapIndex = 1 << childIndex;
		ArrayNode *newNode = new (placement) ArrayNode(bitmapIndex);
		newNode->m_children[0] = newChildNode;

		return newNode;
	}

	static std::uint32_t childIndex(std::uint32_t level, std::uint32_t hashCode)
	{
		return (hashCode >> level) & LevelHashMask;
	}

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

	std::uint32_t childCount() const
	{
		return bitmapPopCount(m_bitmapIndex);
	}

	ArrayNode *withReplacedChild(std::uint32_t childIndex, DatumHashTree *newChildNode)
	{
		ArrayNode *newArrayNode;

		if (newChildNode)
		{
			std::uint32_t newBitmapIndex = m_bitmapIndex | (1 << childIndex);
			newArrayNode = ArrayNode::createInstance(newBitmapIndex);

			for(std::uint32_t i = 0; i < LevelNodeCount; i++)
			{
				if (i == childIndex)
				{
					auto newChildOffset = newArrayNode->childOffsetForIndex(i);
					newArrayNode->m_children[newChildOffset] = newChildNode;
				}
				else if (hasChildAtIndex(i))
				{
					auto oldChildOffset = childOffsetForIndex(i);
					auto newChildOffset = newArrayNode->childOffsetForIndex(i);

					newArrayNode->m_children[newChildOffset] = DatumHashTree::ref(m_children[oldChildOffset]);
				}
			}
		}
		else
		{
			std::uint32_t newBitmapIndex = m_bitmapIndex & ~(1 << childIndex);
			newArrayNode = ArrayNode::createInstance(newBitmapIndex);

			for(std::uint32_t i = 0; i < LevelNodeCount; i++)
			{
				if ((i != childIndex) && hasChildAtIndex(i))
				{
					auto oldChildOffset = childOffsetForIndex(i);
					auto newChildOffset = newArrayNode->childOffsetForIndex(i);

					newArrayNode->m_children[newChildOffset] = DatumHashTree::ref(m_children[oldChildOffset]);
				}
			}
		}

		return newArrayNode;
	}

	DatumHashTree*const* children() const
	{
		return m_children;
	}

private:
	ArrayNode(std::uint32_t bitmapIndex) : DatumHashTree(bitmapIndex)
	{
		assert(bitmapIndex != LeafNodeBitmapIndex);
	}

	bool hasChildAtIndex(std::uint32_t childIndex)
	{
		return m_bitmapIndex & (1 << childIndex);
	}

	std::uint32_t childOffsetForIndex(std::uint32_t childIndex)
	{
		return bitmapPopCount(m_bitmapIndex & ((1 << childIndex) - 1));
	}

	static std::size_t sizeForBitmapIndex(std::uint32_t bitmapIndex)
	{
		return sizeForChildCount(bitmapPopCount(bitmapIndex));
	}

	static std::size_t sizeForChildCount(std::size_t childCount)
	{
		return sizeof(ArrayNode) + (sizeof(DatumHashTree*) * childCount);
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

	AnyCell* findLeaf(AnyCell *key, DatumHash::ResultType hashValue)
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
	 * Adds a new entry to this leaf node
	 *
	 * Tge key must have the same hash value as the existing leaf node
	 */
	LeafNode* assocLeaf(AnyCell *key, DatumHash::ResultType hashValue, AnyCell *value)
	{
		// Check if the key is already in the hash
		for(std::uint32_t i = 0; i < entryCount(); i++)
		{
			LeafNodeEntry &entry = m_entries[i];

			if (key->isEqual(entry.key))
			{
				if (value == entry.value)
				{
					// This value is already in the node; avoid creating a new hash
					ref();
					return this;
				}
				else
				{
					// Create a new leaf node with the value replaced
					LeafNode *newLeafNode = LeafNode::createInstance(hashValue, m_entryCount);
					std::copy(&m_entries[0], &m_entries[m_entryCount], &newLeafNode->entries()[0]);

					newLeafNode->entries()[i].value = value;

					return newLeafNode;
				}
			}
		}

		// No existing value; we need to append
		LeafNode *newLeafNode = LeafNode::createInstance(hashValue, m_entryCount + 1);
		std::copy(&m_entries[0], &m_entries[m_entryCount], &newLeafNode->entries()[0]);

		newLeafNode->entries()[m_entryCount] = {.key = key, .value = value};

		return newLeafNode;
	}

	DatumHashTree* withoutLeaf(AnyCell *key, DatumHash::ResultType hashValue)
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

				LeafNode *newLeafNode = LeafNode::createInstance(hashValue, newEntryCount);

				for(std::uint32_t j = 0; j < i; j++)
				{
					newLeafNode->entries()[j] = entries()[j];
				}

				for(std::uint32_t j = i + 1; j < m_entryCount; j++)
				{
					newLeafNode->entries()[j - 1] = entries()[j];
				}

				return newLeafNode;
			}
		}

		// No value found
		return ref();
	}


private:
	LeafNode(DatumHash::ResultType hashValue, std::uint32_t entryCount) :
		DatumHashTree(LeafNodeBitmapIndex),
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

DatumHashTree::DatumHashTree(std::uint32_t bitmapIndex) :
	m_refCount(1),
	m_bitmapIndex(bitmapIndex)
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
		// We're an empty tree or still have references
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
		delete static_cast<ArrayNode*>(this);
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
	return m_bitmapIndex == LeafNodeBitmapIndex;
}

DatumHashTree* DatumHashTree::createEmpty()
{
	return nullptr;
}

DatumHashTree* DatumHashTree::fromAssocList(ProperList<PairCell> *list)
{
	DatumHashTree *tree = nullptr;
	for(auto pair : *list)
	{
		DatumHashTree *newTree = DatumHashTree::assoc(tree, pair->car(), pair->cdr());
		DatumHashTree::unref(tree);

		tree = newTree;
	}

	return tree;
}

DatumHashTree* DatumHashTree::assoc(DatumHashTree *tree, AnyCell *key, AnyCell *value)
{
	DatumHash hashFunction;
	return assocInternal(tree, 0, key, hashFunction(key), value);
}

AnyCell* DatumHashTree::find(DatumHashTree *tree, AnyCell *key)
{
	DatumHash hashFunction;
	return findInternal(tree, 0, key, hashFunction(key));
}

DatumHashTree* DatumHashTree::without(DatumHashTree *tree, AnyCell *key)
{
	DatumHash hashFunction;
	return withoutInternal(tree, 0, key, hashFunction(key));
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

		const ArrayNode *arrayNode = static_cast<const ArrayNode*>(tree);
		for(std::uint32_t i = 0; i < arrayNode->childCount(); i++)
		{
			accum += size(arrayNode->children()[i]);
		}

		return accum;
	}
}

bool DatumHashTree::every(const DatumHashTree *tree, const std::function<bool(AnyCell*, AnyCell*)> &pred)
{
	if (tree == nullptr)
	{
		return true;
	}
	else if (tree->isLeafNode())
	{
		auto leafNode = static_cast<const LeafNode*>(tree);

		for(std::size_t i = 0; i < leafNode->entryCount(); i++)
		{
			auto &entry = leafNode->entries()[i];
			if (!pred(entry.key, entry.value))
			{
				return false;
			}
		}
	}
	else
	{
		auto arrayNode = static_cast<const ArrayNode*>(tree);

		for(std::uint32_t i = 0; i < arrayNode->childCount(); i++)
		{
			if (!every(arrayNode->children()[i], pred))
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
		auto arrayNode = static_cast<const ArrayNode*>(tree);

		for(std::uint32_t i = 0; i < arrayNode->childCount(); i++)
		{
			walkCellRefs(arrayNode->children()[i], walker, visitor);
		}
	}
}

DatumHashTree* DatumHashTree::assocInternal(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue, AnyCell *value)
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
			return leafNode->assocLeaf(key, hashValue, value);
		}

		// Create an array node with a single value. Note we don't want to directly create a node with two values as
		// both leaf nodes might be assigned the same slot and require a second level array node.
		auto childIndex = ArrayNode::childIndex(level, leafNode->hashValue());
		ArrayNode *newArrayNode = ArrayNode::fromSingleChild(childIndex, leafNode->ref());

		// Note that we don't increase the level here because we're creating a new node on the current level
		DatumHashTree *mergedArrayNode = assocInternal(newArrayNode, level, key, hashValue, value);
		newArrayNode->unref();

		return mergedArrayNode;
	}
	else
	{
		ArrayNode *arrayNode = static_cast<ArrayNode*>(tree);

		auto childIndex = ArrayNode::childIndex(level, hashValue);
		DatumHashTree* childNode = arrayNode->childAtIndex(childIndex);

		DatumHashTree *newChildNode = assocInternal(childNode, level + LevelShiftSize, key, hashValue, value);

		if (childNode == newChildNode)
		{
			// Nothing to do!
			return arrayNode->ref();
		}
		else
		{
			// Allocate a new array node
			return arrayNode->withReplacedChild(childIndex, newChildNode);
		}
	}
}

AnyCell* DatumHashTree::findInternal(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue)
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

		return leafNode->findLeaf(key, hashValue);
	}
	else
	{
		ArrayNode *arrayNode = static_cast<ArrayNode*>(tree);

		DatumHashTree *childNode = arrayNode->childAtIndex(ArrayNode::childIndex(level, hashValue));

		if (childNode == nullptr)
		{
			return nullptr;
		}

		return findInternal(childNode, level + LevelShiftSize, key, hashValue);
	}
}

DatumHashTree* DatumHashTree::withoutInternal(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue)
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

		return leafNode->withoutLeaf(key, hashValue);
	}
	else
	{
		ArrayNode *arrayNode = static_cast<ArrayNode*>(tree);

		auto childIndex = ArrayNode::childIndex(level, hashValue);
		DatumHashTree *childNode = arrayNode->childAtIndex(childIndex);

		if (childNode == nullptr)
		{
			return tree->ref();
		}

		auto newChildNode = withoutInternal(childNode, level + LevelShiftSize, key, hashValue);

		if (childNode == newChildNode)
		{
			// Nothing changed
			DatumHashTree::unref(newChildNode);
			return tree->ref();
		}
		else if ((arrayNode->childCount() == 1) && (newChildNode == nullptr))
		{
			// No more children - return the empty tree
			return nullptr;
		}
		else if ((arrayNode->childCount() == 1) && (newChildNode->isLeafNode()))
		{
			// Single leaf node left - collapse
			return newChildNode;
		}
		else
		{
			return arrayNode->withReplacedChild(childIndex, newChildNode);
		}
	}
}

}
