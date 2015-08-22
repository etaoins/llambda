#ifndef _LLIBY_HASH_DATUMHASHTREE_H
#define _LLIBY_HASH_DATUMHASHTREE_H

#include <cstdint>
#include <atomic>
#include <functional>

#include "hash/DatumHash.h"
#include "binding/ProperList.h"

namespace lliby
{
class AnyCell;

namespace alloc
{
class CellRefWalker;
}

/**
 * Persistent hash tree for mapping data keys to values
 *
 * This uses DatumHash for hashing and AnyCell::isEqual() for equality. DatumHashTrees are persistent; mutating
 * operations preserve the original tree.
 *
 * Memory is managed by internal reference counts which allow derived trees to share subtrees with their source trees.
 * Every function returning a new tree returns a tree with a reference count of 1. These trees can be freed using
 * unref().
 */
class DatumHashTree
{
	friend class alloc::CellRefWalker;
public:
	/**
	 * Creates a new empty tree
	 *
	 * Note that empty trees are represented by a nullptr. Callers should handle nullptr as a valid DatumHashTree.
	 */
	static DatumHashTree* createEmpty()
	{
		return nullptr;
	}

	/**
	 * Creates a tree from an association list
	 *
	 * For each list element the car will be used as the key and the cdr will be used as the value. Duplicate keys will
	 * override previous keys in the list.
	 */
	static DatumHashTree* fromAssocList(ProperList<PairCell> *list);

	/**
	 * Finds a value associated with the specified key
	 *
	 * @param  tree       Tree to find the key in
	 * @param  key        Key to search for
	 * @para,  hashValue  Precomputed hash value for the key. If this is not supplied one will be calculated.
	 * @return Found value to nullptr if the key could not be found
	 */
	static AnyCell *find(DatumHashTree *tree, AnyCell *key, DatumHash::ResultType hashValue);

	static AnyCell *find(DatumHashTree *tree, AnyCell *key)
	{
		DatumHash hasher;
		return find(tree, key, hasher(key));
	}

	/**
	 * Returns a copy of the tree with an additional key/value pair
	 *
	 * @param  tree       Existing tree to add the key/value pair to. This tree will be unmodified.
	 * @param  key        Key to add to the tree. If this key already exists in the hash tree then its value will be
	 *                    replaced. Modifying this key while it is in the hash tree results in undefined behaviour.
	 * @param  value      Value to add to the tree
	 * @para,  hashValue  Precomputed hash value for the key. If this is not supplied one will be calculated.
	 * @return New tree containing the key/value pair
	 */
	static DatumHashTree* assoc(DatumHashTree *tree, AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue);

	static DatumHashTree* assoc(DatumHashTree *tree, AnyCell *key, AnyCell *value)
	{
		DatumHash hasher;
		return assoc(tree, key, value, hasher(key));
	}

	/**
	 * Returns a copy of the tree without the specified key
	 *
	 * @param  tree       Existing tree to remove the key from. This tree will unmodified
	 * @param  key        Key to remove from the tree
	 * @para,  hashValue  Precomputed hash value for the key. If this is not supplied one will be calculated.
	 * @return New tree without the key
	 */
	static DatumHashTree* without(DatumHashTree *tree, AnyCell *key, DatumHash::ResultType hashValue);

	static DatumHashTree* without(DatumHashTree *tree, AnyCell *key)
	{
		DatumHash hasher;
		return without(tree, key, hasher(key));
	}

	/**
	 * Returns the number of entries in the passed tree
	 */
	static std::size_t size(const DatumHashTree *tree);

	/**
	 * Calls the passed function for each {key, value} pair in the tree until it returns false
	 *
	 * The tree will be walked in an undefined order.
	 *
	 * @param  tree  Tree to walk
	 * @param  pred  Predicate function to call for each {key, value} pair. The third argument to this function will be
	 *               the hash value of the key which can be used to speed up DatumHashTree operations. If this functioni
	 *               returns false the walk will be aborted and every() will return false.
	 * @return True if the walker returned true for every {key, value} pair
	 */
	static bool every(const DatumHashTree *tree, const std::function<bool(AnyCell*, AnyCell*, DatumHash::ResultType)> &pred);

	/**
	 * Increases the reference count of the passed tree and returns it
	 *
	 * This can be considered semantically equivalent to copying the tree.
	 *
	 * @param  tree  Tree to increase the reference count for.
	 * @return The passed tree
	 */
	static DatumHashTree* ref(DatumHashTree *tree);

	/**
	 * Decreases the reference count of the passed tree
	 *
	 * If this is the last reference to the tree it will be freed.
	 *
	 * @param  tree  Tree to decrease the reference count for
	 */
	static void unref(DatumHashTree *tree);

	/**
	 * Number of DatumHashTree instances including internal subtrees
	 *
	 * If leak checking is disabled this always returns 0
	 *
	 * This is not synchronized with other threads. For that reason this value is only accurate when there is no
	 * concurrent instance creation or destruction and any other previously modifying threads have been synchronized
	 * with through another mechanism.
	 */
	static std::size_t instanceCount();

protected:
	static void walkCellRefs(DatumHashTree *tree, alloc::CellRefWalker &walker, const std::function<void(AnyCell**, AnyCell**)> &visitor);

protected:
	explicit DatumHashTree(std::uint32_t bitmapIndex);
	~DatumHashTree();

	void *operator new(size_t s, void *placement);
	void operator delete(void *value);

	DatumHashTree* ref();
	void unref();

	bool isLeafNode() const;

	static DatumHashTree* assocAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue, bool inPlace = false);
	static AnyCell* findAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValue);
	static DatumHashTree* withoutAtLevel(DatumHashTree *tree, std::uint32_t level, AnyCell *key, DatumHash::ResultType hashValueg);

	static const std::uint32_t LeafNodeChildBitmap = 0;

	std::atomic<std::uint32_t> m_refCount;
	const std::uint32_t m_childBitmap;
};


}


#endif
