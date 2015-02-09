#ifndef _LLIBY_PORT_STANDARDOUTPUTPORT_H
#define _LLIBY_PORT_STANDARDOUTPUTPORT_H

#include "AbstractPort.h"

#include <unistd.h>
#include <atomic>

namespace lliby
{

class StandardOutputPort : public AbstractOutputOnlyPort
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
		int toClose = m_fd;
		if (m_fd.compare_exchange_strong(toClose, ClosedFd))
		{
			close(toClose);
		}
	}

	std::ostream *outputStream() override
	{
		return &m_outputStream;
	}

private:
	std::ostream &m_outputStream;
	std::atomic<int> m_fd;
};

}

#endif
