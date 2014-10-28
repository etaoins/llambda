#ifndef _LLIBY_PORT_STANDARDOUTPUTPORT_H
#define _LLIBY_PORT_STANDARDOUTPUTPORT_H

#include "AbstractPort.h"

#include <unistd.h>

namespace lliby
{

class StandardOutputPort : public AbstractOutputPort
{
	static const int ClosedFd = -1;
public:
	StandardOutputPort(std::ostream &outputStream, int fd) :
		m_outputStream(outputStream),
		m_fd(fd)
	{
	}

	bool isOutputPortOpen() const override
	{
		return m_fd != ClosedFd;
	}

	void closeOutputPort() override
	{
		if (isOutputPortOpen())
		{
			m_outputStream.flush();

			close(m_fd);
			m_fd = ClosedFd;
		}
	}

	std::ostream *outputStream() override
	{
		return &m_outputStream;
	}

private:
	std::ostream &m_outputStream;
	int m_fd;
};

}

#endif
