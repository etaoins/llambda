#include "fatal.h"

#include <iostream>
#include <unistd.h>

#include "core/init.h"

#include "binding/DatumCell.h"
#include "binding/ProperList.h"
#include "binding/StringCell.h"
#include "binding/ErrorObjectCell.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

void _lliby_fatal(const char *message, const DatumCell *evidence)
{
	std::cerr << message << std::endl;

	if (evidence) 
	{
		ExternalFormDatumWriter writer(std::cerr);

		if (auto errorCell = datum_cast<ErrorObjectCell>(evidence))
		{
			// Handle error objects specially
			std::cerr << "Message: ";
			writer.render(errorCell->message());
			std::cerr << std::endl;

			ProperList<DatumCell> irritantList(errorCell->irritants());

			if (irritantList.length() == 1)
			{
				// Special case a single irritant (the usual)
				std::cerr << "Irritant: ";
				writer.render(*irritantList.begin());
				std::cerr << std::endl;
			}
			else if (irritantList.length() > 1)
			{
				std::cerr << "Irritants: ";
				writer.render(errorCell->irritants());
				std::cerr << std::endl;
			}
		}
		else
		{
			std::cerr << "Evidence: ";
			writer.render(evidence);
			std::cerr << std::endl;
		}
	}

	exit(-1);
	__builtin_unreachable();
}

}
