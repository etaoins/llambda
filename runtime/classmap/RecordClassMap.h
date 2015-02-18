#ifndef _LLIBY_CLASSMAP_RECORDCLASSMAP_H
#define _LLIBY_CLASSMAP_RECORDCLASSMAP_H

extern "C"
{

struct RecordClassMap
{
	std::uint32_t totalSize;
	std::uint32_t offsetCount;
	std::uint32_t offsets[];
};

extern const RecordClassMap *_llambda_compiler_class_map[];

}

#endif
