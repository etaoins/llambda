#include <sys/stat.h>
#include <memory>

#include "binding/StringCell.h"
#include "binding/PortCell.h"

#include "core/error.h"

#include "port/FileInputPort.h"

using namespace lliby;

extern "C"
{

bool lliby_file_exists(StringCell *filePath)
{
	struct stat statBuf;

	return stat(filePath->toUtf8StdString().c_str(), &statBuf) == 0;
}

PortCell* lliby_open_input_file(World &world, StringCell *filePath)
{
	auto inputPort = new FileInputPort(filePath->toUtf8StdString());

	if (!inputPort->inputStream()->good())
	{
		delete inputPort;
		signalError(world, "Unable to open path for reading", {filePath}, ErrorCategory::File);
	}

	return PortCell::createInstance(world, inputPort);
}

}
