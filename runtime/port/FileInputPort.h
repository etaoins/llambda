#ifndef _LLIBY_PORT_FILEINPUTPORT_H
#define _LLIBY_PORT_FILEINPUTPORT_H

#include "AbstractPort.h"

#include <fstream>

namespace lliby
{

class FileInputPort : public AbstractInputPort
{
public:
	FileInputPort(const std::string &path) :
		m_stream(path)
	{
	}

	bool isInputPortOpen() const override
	{
		return m_stream.is_open();
	}

	void closeInputPort() override
	{
		m_stream.close();
	}

	std::ifstream *inputStream() override
	{
		return &m_stream;
	}

protected:
	std::ifstream m_stream;
};

}

#endif
