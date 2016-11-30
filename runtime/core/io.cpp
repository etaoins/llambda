#include <iostream>

#include <unistd.h>

#include "binding/PortCell.h"
#include "writer/ExternalFormDatumWriter.h"

#include "port/StandardInputPort.h"
#include "port/StandardOutputPort.h"

using namespace lliby;

extern "C"
{

PortCell *llcore_stdout_port()
{
	static PortCell constantStdout(new StandardOutputPort(std::cout, STDOUT_FILENO), GarbageState::GlobalConstant);
	return &constantStdout;
}

PortCell *llcore_stderr_port()
{
	static PortCell constantStderr(new StandardOutputPort(std::cerr, STDERR_FILENO), GarbageState::GlobalConstant);
	return &constantStderr;
}

PortCell *llcore_stdin_port()
{
	static PortCell constantStdin(new StandardInputPort(std::cin, STDIN_FILENO), GarbageState::GlobalConstant);
	return &constantStdin;
}

void llcore_write_stdout(AnyCell *datum)
{
	ExternalFormDatumWriter writer(std::cout);
	writer.render(datum);
}

}

