#ifndef _LLIBY_PORT_FILEOUTPUTPORT_H
#define _LLIBY_PORT_FILEOUTPUTPORT_H

#include "AbstractPort.h"

#include <fstream>

namespace lliby
{

class FileOutputPort : public AbstractOutputPort
{
public:
	FileOutputPort(const std::string &path) :
		m_stream(path)
	{
	}

	bool isOutputPortOpen() const override
	{
		return m_stream.is_open();
	}

	void closeOutputPort() override
	{
		m_stream.close();
	}

	std::ofstream *outputStream() override
	{
		return &m_stream;
	}

protected:
	std::ofstream m_stream;
};

}

#endif
