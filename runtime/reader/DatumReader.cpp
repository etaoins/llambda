#include "DatumReader.h"

#include "binding/EofObjectCell.h"

namespace lliby
{

AnyCell* DatumReader::parse(int defaultRadix)
{
	return EofObjectCell::instance();
}

}
