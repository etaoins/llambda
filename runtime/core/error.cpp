#include "core/error.h"
#include "core/World.h"

#include "alloc/cellref.h"

#include <unistd.h>
#include <iostream>
#include <sstream>

#include "binding/StringCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/ProperList.h"

#include "dynamic/SchemeException.h"

#include "writer/ExternalFormDatumWriter.h"

extern "C"
{

using namespace lliby;

void _lliby_signal_error(World &world, const char *message, AnyCell *irritant, const char *path, unsigned int lineNumber)
{
	if (irritant != nullptr)
	{
		signalError(world, message, {irritant}, path, lineNumber);
	}
	else
	{
		signalError(world, message, {});
	}
}

}

namespace lliby
{

void signalError(World &world, const char *message, const std::vector<AnyCell*> &irritants, const char *path, unsigned int lineNumber)
{
	// Build our full message string
	std::ostringstream messageStream;
	messageStream << message;

	if (path != nullptr)
	{
		messageStream << " at " << path << ":" << lineNumber;
	}

	// Convert our C++ data type to Scheme cells
	alloc::StrongRef<ProperList<AnyCell>> irritantsCell(world, ProperList<AnyCell>::create(world, irritants));
	StringCell *messageCell = StringCell::fromUtf8StdString(world, messageStream.str());

	// Throw a new exception
	auto errorObj = ErrorObjectCell::createInstance(world, messageCell, irritantsCell);
	throw dynamic::SchemeException(errorObj);
}

void fatalError(const char *message, const AnyCell *evidence)
{
	std::cerr << message << std::endl;

	if (evidence)
	{
		ExternalFormDatumWriter writer(std::cerr);

		if (auto errorCell = cell_cast<ErrorObjectCell>(evidence))
		{
			// Handle error objects specially
			std::cerr << "Message: " << errorCell->message() << std::endl;

			ProperList<AnyCell> *irritants = errorCell->irritants();
			auto irritantCount = irritants->size();

			if (irritantCount == 1)
			{
				// Special case a single irritant (the usual)
				std::cerr << "Irritant: ";
				writer.render(*irritants->begin());
				std::cerr << std::endl;
			}
			else if (irritantCount > 1)
			{
				std::cerr << "Irritants: ";
				writer.render(irritants);
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
