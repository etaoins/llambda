#ifndef _LLIBY_PORT_BUFFERINPUTPORT_H
#define _LLIBY_PORT_BUFFERINPUTPORT_H

#include "AbstractPort.h"

#include <sstream>

namespace lliby
{

class BufferInputPort : public AbstractInputPort
{
public:
	BufferInputPort(const std::string &inputString) :
		m_buffer(inputString)
	{
	}

	bool isInputPortOpen() const override
	{
		return m_open;
	}

	void closeInputPort() override
	{
		m_open = false;
	}

	std::istream *inputStream() override
	{
		return &m_buffer;
	}

protected:
	bool m_open = true;
	std::istringstream m_buffer;
};

}

#endif
