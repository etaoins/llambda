#ifndef _LLIBY_CLASSMAP_RECORDCLASSMAP_H
#define _LLIBY_CLASSMAP_RECORDCLASSMAP_H

extern "C" 
{

struct RecordClassOffsetMap
{
	std::uint32_t offsetCount;
	std::uint32_t offsets[];
};

extern const RecordClassOffsetMap *_llambda_class_map[];

}

#endif
