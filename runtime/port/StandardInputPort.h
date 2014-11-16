#ifndef _LLIBY_PORT_STANDARDINPUTPORT_H
#define _LLIBY_PORT_STANDARDINPUTPORT_H

#include "AbstractPort.h"

#include <unistd.h>

namespace lliby
{

class StandardInputPort : public AbstractInputOnlyPort
{
	static const int ClosedFd = -1;
public:
	StandardInputPort(std::istream &inputStream, int fd) :
		m_inputStream(inputStream),
		m_fd(fd)
	{
	}

	bool isInputPortOpen() const override
	{
		return m_fd != ClosedFd;
	}

	void closeInputPort() override
	{
		if (isInputPortOpen())
		{
			close(m_fd);
			m_fd = ClosedFd;
		}
	}

	std::istream *inputStream() override
	{
		return &m_inputStream;
	}

private:
	std::istream &m_inputStream;
	int m_fd;
};

}

#endif
