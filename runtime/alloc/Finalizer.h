#ifndef _LLIBY_ALLOC_FINALIZER_H
#define _LLIBY_ALLOC_FINALIZER_H

namespace lliby
{
namespace alloc
{

class Heap;
class MemoryBlock;

// Although this only contains static functions it needs to be a class so it can be a friend of Heap
class Finalizer
{
public:
	static void finalizeHeapAsync(Heap &heap);
	static void finalizeHeapSync(Heap &heap);

private:
	static void finalizeSegment(MemoryBlock *rootSegment);
	static void terminateHeap(Heap &heap);
};

}
}

#endif
