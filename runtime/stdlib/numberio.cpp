#include <sstream>
#include <cmath>

#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"

#include "writer/ExternalFormDatumWriter.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

StringCell* lliby_number_to_string(World &world, NumberCell *numberCell, std::uint8_t radix)
{
	if ((radix != 2) && (radix != 8) && (radix != 10) && (radix != 16))
	{
		signalError(world, "(number->string) with illegal radix", {numberCell});
	}

	std::ostringstream strStream;
	ExternalFormDatumWriter writer(strStream);

	if (auto flonumCell = cell_cast<FlonumCell>(numberCell))
	{
		const double floatValue = flonumCell->value();

		// R7RS allows us to render +nan.0, -inf.0 and +inf.0 in non-decimal radix
		if ((!std::isnan(floatValue) && !std::isinf(floatValue)) && (radix != 10))
		{
			signalError(world, "(number->string) with inexact number and non-decimal radix", {numberCell});
		}
	}

	writer.render(numberCell, radix);
	return StringCell::fromUtf8StdString(world, strStream.str());
}

}
