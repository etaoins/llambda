#include "fatal.h"

#include <iostream>
#include <unistd.h>

#include "binding/DatumCell.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

void _lliby_fatal(const char *message, const DatumCell *evidence)
{
	if (evidence) 
	{
		ExternalFormDatumWriter writer(std::cerr);

		std::cerr << "Datum: ";
		writer.render(evidence);
		std::cerr << std::endl;
	}

	std::cerr << message << std::endl;
	exit(-1);
	__builtin_unreachable();
}

}
