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

void llcore_signal_error(World &world, const char *message, AnyCell *irritant, const char *path, unsigned int lineNumber)
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
		signalError(world, ErrorCategory::Default, messageStr.c_str(), {irritant});
	}
	else
	{
		signalError(world, ErrorCategory::Default, messageStr.c_str(), {});
	}
}

}

namespace lliby
{

void signalError(World &world, ErrorCategory category, const char *message, const std::vector<AnyCell*> &irritants)
{
	// Convert our C++ data type to Scheme cells
	std::vector<AnyCell*> irritantsCopy(irritants);
	alloc::StrongRef<ProperList<AnyCell>> irritantsCell(world, ProperList<AnyCell>::create(world, irritantsCopy));

	StringCell *messageCell = StringCell::fromUtf8StdString(world, message);

	// Throw a new exception
	auto errorObj = ErrorObjectCell::createInstance(world, messageCell, irritantsCell, category);
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
			switch(errorCell->category())
			{
			case ErrorCategory::Default:
				break;
			case ErrorCategory::File:
				std::cerr << "Category: File" << std::endl;
				break;
			case ErrorCategory::Read:
				std::cerr << "Category: Read" << std::endl;
				break;
			case ErrorCategory::Type:
				std::cerr << "Category: Type" << std::endl;
				break;
			case ErrorCategory::Arity:
				std::cerr << "Category: Arity" << std::endl;
				break;
			case ErrorCategory::Range:
				std::cerr << "Category: Range" << std::endl;
				break;
			case ErrorCategory::Utf8:
				std::cerr << "Category: UTF-8" << std::endl;
				break;
			case ErrorCategory::DivideByZero:
				std::cerr << "Category: Divide-by-zero" << std::endl;
				break;
			case ErrorCategory::MutateLiteral:
				std::cerr << "Category: Literal mutation" << std::endl;
				break;
			case ErrorCategory::UndefinedVariable:
				std::cerr << "Category: Undefined variable" << std::endl;
				break;
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
