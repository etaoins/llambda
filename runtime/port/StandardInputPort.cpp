#include "StandardInputPort.h"

#include <poll.h>

namespace lliby
{

bool StandardInputPort::bytesAvailable() const
{
	// Are there any buffered bytes?
	if (m_inputStream.rdbuf()->in_avail() > 0)
	{
		return true;
	}

	// No, check the underlying file descriptor
	struct pollfd pollInfo;
	pollInfo.fd = m_fd;
	pollInfo.events = POLLIN;

	return poll(&pollInfo, 1, 0) > 0;
}

}
