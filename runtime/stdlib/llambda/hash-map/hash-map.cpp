#include "core/World.h"

#include "binding/HashMapCell.h"
#include "binding/ProperList.h"
#include "binding/PairCell.h"
#include "binding/TypedProcedureCell.h"

#include "hash/DatumHashTree.h"
#include "alloc/cellref.h"

extern "C"
{

using namespace lliby;

HashMapCell *llhashmap_make_hash_map(World &world)
{
	return HashMapCell::createEmptyInstance(world);
}

std::int64_t llhashmap_hash_map_size(HashMapCell *hashMap)
{
	return DatumHashTree::size(hashMap->datumHashTree());
}

HashMapCell *llhashmap_hash_map_assoc(World &world, HashMapCell *basisRaw, AnyCell *rawKey, AnyCell *rawValue)
{
	alloc::HashMapRef basis(world, basisRaw);
	alloc::AnyRef key(world, rawKey);
	alloc::AnyRef value(world, rawValue);

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

HashMapCell *llhashmap_hash_map_delete(World &world, HashMapCell *basisRaw, AnyCell *keyRaw)
{
	alloc::HashMapRef basis(world, basisRaw);
	alloc::AnyRef key(world, keyRaw);
	HashMapCell *newHashMap = HashMapCell::createEmptyInstance(world);

	newHashMap->setDatumHashTree(DatumHashTree::without(basis->datumHashTree(), key));
	return newHashMap;
}

HashMapCell *llhashmap_alist_to_hash_map(World &world, ProperList<PairCell> *alistRaw)
{
	alloc::StrongRef<ProperList<PairCell>> alist(world, alistRaw);
	HashMapCell *newHashMap = HashMapCell::createEmptyInstance(world);

	newHashMap->setDatumHashTree(DatumHashTree::fromAssocList(alist));
	return newHashMap;
}

ProperList<PairCell> *llhashmap_hash_map_to_alist(World &world, HashMapCell *hashMapRaw)
{
	alloc::HashMapRef hashMap(world, hashMapRaw);
	alloc::StrongRefVector<PairCell> assocPairs(world);

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value)
	{
		assocPairs.push_back(PairCell::createInstance(world, key, value));
		return true;
	});

	return ProperList<PairCell>::create(world, assocPairs);
}

ProperList<AnyCell> *llhashmap_hash_map_keys(World &world, HashMapCell *hashMapRaw)
{
	alloc::HashMapRef hashMap(world, hashMapRaw);
	alloc::StrongRefVector<AnyCell> keys(world);

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value)
	{
		keys.push_back(key);
		return true;
	});

	return ProperList<AnyCell>::create(world, keys);
}

ProperList<AnyCell> *llhashmap_hash_map_values(World &world, HashMapCell *hashMapRaw)
{
	alloc::HashMapRef hashMap(world, hashMapRaw);
	alloc::StrongRefVector<AnyCell> values(world);

	DatumHashTree::every(hashMap->datumHashTree(), [&] (AnyCell *key, AnyCell *value)
	{
		values.push_back(value);
		return true;
	});

	return ProperList<AnyCell>::create(world, values);
}

}
