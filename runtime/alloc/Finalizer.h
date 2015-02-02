#ifndef _LLIBY_ALLOC_FINALIZER_H
#define _LLIBY_ALLOC_FINALIZER_H

namespace lliby
{
namespace alloc
{

class Heap;
class MemoryBlock;

class Finalizer
{
public:
	void finalizeHeapAsync(Heap &heap);
	static void finalizeHeapSync(Heap &heap);

private:
	static void finalizeSegment(MemoryBlock *rootSegment);
	static void terminateHeap(Heap &heap);
};

}
}

#endif
