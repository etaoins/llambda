#ifndef _LLIBY_UTIL_PORTCELLTOSTREAM_H
#define _LLIBY_UTIL_PORTCELLTOSTREAM_H

#include <iostream>

#include "binding/PortCell.h"

using namespace lliby;

namespace lliby
{
class World;

std::ostream* portCellToOutputStream(World &world, PortCell *portCell);
std::istream* portCellToInputStream(World &world, PortCell *portCell);

}

#endif
