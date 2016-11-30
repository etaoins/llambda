#include <sstream>
#include <cmath>

#include "binding/IntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"
#include "binding/BooleanCell.h"
#include "binding/EofObjectCell.h"

#include "writer/ExternalFormDatumWriter.h"
#include "reader/DatumReader.h"
#include "reader/ReadErrorException.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

StringCell* llbase_number_to_string(World &world, NumberCell *numberCell, std::uint8_t radix)
{
	if ((radix != 2) && (radix != 8) && (radix != 10) && (radix != 16))
	{
		signalError(world, ErrorCategory::InvalidArgument, "(number->string) with illegal radix", {numberCell});
	}

	std::ostringstream strStream;
	ExternalFormDatumWriter writer(strStream);

	if (auto flonumCell = cell_cast<FlonumCell>(numberCell))
	{
		const double floatValue = flonumCell->value();

		// R7RS allows us to render +nan.0, -inf.0 and +inf.0 in non-decimal radix
		if ((!std::isnan(floatValue) && !std::isinf(floatValue)) && (radix != 10))
		{
			signalError(world, ErrorCategory::InvalidArgument, "(number->string) with flonum and non-decimal radix", {numberCell});
		}
	}

	writer.render(numberCell, radix);
	return StringCell::fromUtf8StdString(world, strStream.str());
}

AnyCell* llbase_string_to_number(World &world, StringCell *stringCell, std::uint32_t radix)
{
	if ((radix != 2) && (radix != 8) && (radix != 10) && (radix != 16))
	{
		signalError(world, ErrorCategory::InvalidArgument, "(string->number) with illegal radix", {stringCell});
	}

	std::string inputString(stringCell->toUtf8StdString());
	std::istringstream strStream(inputString);

	try
	{
		DatumReader reader(world, strStream);

		if (auto numberCell = cell_cast<NumberCell>(reader.parse(radix)))
		{
			if (strStream.get() != EOF)
			{
				// Junk after number
				return BooleanCell::falseInstance();
			}

			return numberCell;
		}

		return BooleanCell::falseInstance();
	}
	catch(ReadErrorException)
	{
		return BooleanCell::falseInstance();
	}
}

}
