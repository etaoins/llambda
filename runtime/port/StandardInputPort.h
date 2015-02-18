#ifndef _LLIBY_PORT_STANDARDINPUTPORT_H
#define _LLIBY_PORT_STANDARDINPUTPORT_H

#include "AbstractPort.h"

#include <unistd.h>
#include <atomic>

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
		int toClose = m_fd;
		if (m_fd.compare_exchange_strong(toClose, ClosedFd))
		{
			close(toClose);
		}
	}

	bool bytesAvailable() const override;

	std::istream *inputStream() override
	{
		return &m_inputStream;
	}

private:
	std::istream &m_inputStream;
	std::atomic<int> m_fd;
};

}

#endif
