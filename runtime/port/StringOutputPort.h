#ifndef _LLIBY_PORT_STRINGOUTPUTPORT_H
#define _LLIBY_PORT_STRINGOUTPUTPORT_H

#include "BufferOutputPort.h"

#include "binding/StringCell.h"

namespace lliby
{

class StringOutputPort : public BufferOutputPort
{
public:
	StringCell *outputToStringCell(World &world)
	{
		return StringCell::fromUtf8StdString(world, m_buffer.str());
	}
};

}


#endif
