#include "core/error.h"
#include "core/World.h"

#include "alloc/StrongRef.h"

#include <unistd.h>
#include <iostream>

#include "binding/StringCell.h"
#include "binding/ListElementCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/ProperList.h"

#include "dynamic/SchemeException.h"

#include "writer/ExternalFormDatumWriter.h"

extern "C"
{

using namespace lliby;

void _lliby_fatal(const char *message, const DatumCell *evidence)
{
	fatalError(message, evidence);
}

void _lliby_signal_error(World &world, const char *message, DatumCell *irritant)
{
	if (irritant != nullptr)
	{
		signalError(world, message, {irritant});
	}
	else
	{
		signalError(world, message, {});
	}
}

}

namespace lliby
{

void signalError(World &world, const char *message, const std::vector<DatumCell*> &irritants)
{
	// Convert our C++ data type to Scheme cells
	alloc::StrongRef<ListElementCell> irritantsCell(world, ListElementCell::createProperList(world, irritants));
	alloc::StrongRef<StringCell> messageCell(world, StringCell::fromUtf8CString(message));

	// Throw a new exception
	auto errorObj = ErrorObjectCell::createInstance(world, messageCell, irritantsCell);
	throw dynamic::SchemeException(world, errorObj);
}

void fatalError(const char *message, const DatumCell *evidence)
{
	std::cerr << message << std::endl;

	if (evidence) 
	{
		ExternalFormDatumWriter writer(std::cerr);

		if (auto errorCell = datum_cast<ErrorObjectCell>(evidence))
		{
			// Handle error objects specially
			std::cerr << "Message: " << errorCell->message()->utf8Data() << std::endl;

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
