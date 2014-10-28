#ifndef _LLIBY_PORT_BYTEVECTOROUTPUTPORT_H
#define _LLIBY_PORT_BYTEVECTOROUTPUTPORT_H

#include "BufferOutputPort.h"

#include "binding/BytevectorCell.h"

namespace lliby
{

class BytevectorOutputPort : public BufferOutputPort
{
public:
	BytevectorCell *outputToBytevectorCell(World &world)
	{
		std::string outputString(m_buffer.str());

		auto outputData = reinterpret_cast<const std::uint8_t*>(outputString.data());
		auto outputSize = outputString.size();

		return BytevectorCell::fromData(world, outputData, outputSize);
	}
};

}


#endif
