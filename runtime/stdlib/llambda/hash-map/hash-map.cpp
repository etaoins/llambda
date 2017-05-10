#include "core/World.h"

#include "binding/HashMapCell.h"
#include "binding/ProperList.h"
#include "binding/PairCell.h"
#include "binding/TypedProcedureCell.h"

#include "hash/DatumHashTree.h"
#include "hash/DatumHash.h"

extern "C"
{

using namespace lliby;

using FoldProc = TypedProcedureCell<AnyCell*, AnyCell*, AnyCell*, AnyCell*>;
using DefaultProc = TypedProcedureCell<AnyCell*>;

HashMapCell *llhashmap_make_hash_map(World &world)
{
	return HashMapCell::createEmptyInstance(world);
}

std::int64_t llhashmap_hash_map_size(HashMapCell *hashMap)
{
	return DatumHashTree::size(hashMap->datumHashTree());
}

HashMapCell *llhashmap_hash_map_assoc(World &world, HashMapCell *basis, AnyCell *key, AnyCell *value)
{
	HashMapCell *newHashMap = HashMapCell::createEmptyInstance(world);

	newHashMap->setDatumHashTree(DatumHashTree::assoc(basis->datumHashTree(), key, value));
	return newHashMap;
}

bool llhashmap_hash_map_exists(HashMapCell *hashMap, AnyCell *key)
{
	return DatumHashTree::find(hashMap->datumHashTree(), key) != nullptr;
}

AnyCell *llhashmap_hash_map_ref_default(HashMapCell *hashMap, AnyCell *key, AnyCell *defaultValue)
{
	AnyCell *treeValue = DatumHashTree::find(hashMap->datumHashTree(), key);

	if (treeValue == nullptr)
	{
		return defaultValue;
	}
	else
	{
		return treeValue;
	}
}

AnyCell *llhashmap_hash_map_ref(World &world, HashMapCell *hashMap, AnyCell *key, DefaultProc *defaultProc)
{
	AnyCell *treeValue = DatumHashTree::find(hashMap->datumHashTree(), key);

	if (treeValue == nullptr)
	{
		return defaultProc->apply(world);
	}
	else
	{
		return treeValue;
	}
}

HashMapCell *llhashmap_hash_map_delete(World &world, HashMapCell *basis, AnyCell *key)
{
	HashMapCell *newHashMap = HashMapCell::createEmptyInstance(world);

	newHashMap->setDatumHashTree(DatumHashTree::without(basis->datumHashTree(), key));
	return newHashMap;
}

HashMapCell *llhashmap_alist_to_hash_map(World &world, ProperList<PairCell> *alist)
{
	HashMapCell *newHashMap = HashMapCell::createEmptyInstance(world);

	newHashMap->setDatumHashTree(DatumHashTree::fromAssocList(alist));
	return newHashMap;
}

ProperList<PairCell> *llhashmap_hash_map_to_alist(World &world, HashMapCell *hashMap)
{
	std::vector<PairCell*> assocPairs;

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType)
	{
		assocPairs.push_back(PairCell::createInstance(world, key, value));
		return true;
	});

	return ProperList<PairCell>::create(world, assocPairs);
}

ProperList<AnyCell> *llhashmap_hash_map_keys(World &world, HashMapCell *hashMap)
{
	std::vector<AnyCell*> keys;

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType)
	{
		keys.push_back(key);
		return true;
	});

	return ProperList<AnyCell>::create(world, keys);
}

ProperList<AnyCell> *llhashmap_hash_map_values(World &world, HashMapCell *hashMap)
{
	std::vector<AnyCell*> values;

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType)
	{
		values.push_back(value);
		return true;
	});

	return ProperList<AnyCell>::create(world, values);
}

void llhashmap_hash_map_for_each(World &world, TypedProcedureCell<void, AnyCell *, AnyCell *> *walker, HashMapCell *hashMap)
{
	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType)
	{
		walker->apply(world, key, value);
		return true;
	});
}

AnyCell* llhashmap_hash_map_fold(World &world, FoldProc *folder, AnyCell *initialValue, HashMapCell *hashMap)
{
	AnyCell *accum = initialValue;

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType)
	{
		accum = folder->apply(world, key, value, accum);
		return true;
	});

	return accum;
}

HashMapCell* llhashmap_hash_map_merge(World &world, HashMapCell *sourceHashMap, HashMapCell *overrideHashMap)
{
	DatumHashTree *resultTree = DatumHashTree::ref(sourceHashMap->datumHashTree());
	void *placement = alloc::allocateCells(world);

	DatumHashTree::every(overrideHashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue)
	{
		DatumHashTree *newTree = DatumHashTree::assoc(resultTree, key, value, hashValue);
		DatumHashTree::unref(resultTree);

		resultTree = newTree;

		return true;
	});

	return new (placement) HashMapCell(resultTree);
}

std::uint32_t llhashmap_hash(AnyCell *datum, std::int64_t bound)
{
	DatumHash hasher;
	return hasher(datum) % bound;
}

}
