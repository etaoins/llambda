#ifndef _LLIBY_READER_DATUMREADER_H
#define _LLIBY_READER_DATUMREADER_H

#include <istream>

#include "binding/generated/declaretypes.h"

namespace lliby
{

class World;

class DatumReader
{
public:
	DatumReader(World &world, std::istream &inStream) :
		m_world(world),
		m_inStream(inStream)
	{
	}

	/**
	 * Parses a datum in external form
	 *
	 * If the end of input is reached then EofObjectCell::instance() is returned. As the EOF object has no external form
	 * this is an unambiguous indicator of the end of file.
	 *
	 * If the end of input is reached in the middle of the datum then ReadErrorException will be thrown
	 *
	 * @param  datum  Radix to parse unprefixed numbers in. This not apply to nested data wnich uses the default radix
	 *                of 10
	 */
	AnyCell* parse(int defaultRadix = 10);

protected:
	AnyCell *parseOctoDatum();
	AnyCell *parseEnclosedSymbol();
	AnyCell *parseString();
	AnyCell *parseSymbol();

	AnyCell *parseNumber(int radix);
	AnyCell *parsePositiveNumber(int radix);
	AnyCell *parseNegativeNumber(int radix);
	AnyCell *parseUnradixedNumber(int radix, bool negative = false);

	AnyCell *parseList(char closeChar);

	World &m_world;
	std::istream &m_inStream;
};

}


#endif
