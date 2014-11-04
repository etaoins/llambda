#ifndef _LLIBY_READER_DATUMREADER_H
#define _LLIBY_READER_DATUMREADER_H

#include <ostream>

#include "binding/generated/declaretypes.h"

namespace lliby
{

class World;

class DatumReader
{
public:
	DatumReader(World &world, std::istream &inStream) :
		m_world(world),
		m_inStream(inStream)
	{
	}

	AnyCell* parse(int defaultRadix = 10);

protected:
	World &m_world;
	std::istream &m_inStream;
};

}


#endif
