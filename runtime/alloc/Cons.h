#ifndef _LLIBY_ALLOC_CONS_H
#define _LLIBY_ALLOC_CONS_H

#include "binding/BoxedPair.h"

namespace lliby
{
namespace alloc
{

// This is a placeholder for size purposes
// We assume a BoxedPair is the largest allocation with two pointers
class Cons : public BoxedPair
{
};

}
}

#endif

