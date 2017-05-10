#include "core/error.h"
#include "core/World.h"

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

void llcore_signal_error(World &world, ErrorCategory category, const char *message, AnyCell *irritant, const char *path, unsigned int lineNumber)
{
	std::string messageStr;

	if (path != nullptr)
	{
		std::ostringstream messageStream;

		messageStream << message;
		messageStream << " at " << path << ":" << lineNumber;

		messageStr = messageStream.str();
	}
	else
	{
		messageStr = message;
	}

	if (irritant != nullptr)
	{
		signalError(world, category, messageStr, {irritant});
	}
	else
	{
		signalError(world, category, messageStr, {});
	}
}

}

namespace lliby
{

void signalError(World &world, ErrorCategory category, const std::string &message, std::initializer_list<AnyCell*> irritants)
{
	// Convert our C++ data type to Scheme cells
	ProperList<AnyCell> *irritantsCell = ProperList<AnyCell>::create(world, irritants);

	StringCell *messageCell = StringCell::fromUtf8StdString(world, message);

	// Throw a new exception
	auto errorObj = ErrorObjectCell::createInstance(world, messageCell, irritantsCell, category);
	throw dynamic::SchemeException(errorObj);
}

void fatalError(const std::string &message, const AnyCell *evidence)
{
	std::cerr << message << std::endl;

	if (evidence)
	{
		ExternalFormDatumWriter writer(std::cerr);

		if (auto errorCell = cell_cast<ErrorObjectCell>(evidence))
		{
			if (errorCell->category() != ErrorCategory::Default)
			{
				std::cerr << "Category: " << schemeNameForErrorCategory(errorCell->category()) << std::endl;
			}

			// Handle error objects specially
			std::cerr << "Message:  " << errorCell->message() << std::endl;

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
