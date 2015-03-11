#ifndef _LLIBY_READER_DATUMREADER_H
#define _LLIBY_READER_DATUMREADER_H

#include <istream>
#include <string>
#include <unordered_map>

#include "alloc/StrongRef.h"
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
	 * If the end of input is reached within a datum or syntatically invalid data is provided then ReadErrorException
	 * will be thrown. If an invalid UTF-8 enoding is detected the utf8::InvalidByteSequenceException may be thrown.
	 *
	 * @param  defaultRadix  Radix to parse unprefixed numbers in. This not apply to nested data wnich uses the default
	 *                       radix of 10
	 */
	AnyCell* parse(int defaultRadix = 10);

protected:
	std::streambuf* rdbuf()
	{
		return m_inStream.rdbuf();
	}

	int consumeWhitespace();
	void consumeBlockComment();

	AnyCell *parseDatum(int defaultRadix = 10);

	AnyCell *parseOctoDatum();
	AnyCell *parseEnclosedSymbol();
	AnyCell *parseString();
	AnyCell *parseSymbol();
	AnyCell *parseSymbolShorthand(const std::string &expanded);
	AnyCell *parseChar();

	AnyCell *parseNumber(int radix);
	AnyCell *parsePositiveNumber(int radix);
	AnyCell *parseNegativeNumber(int radix);
	AnyCell *parseUnradixedNumber(int radix, bool negative = false);

	AnyCell *parseList(char closeChar);
	AnyCell *parseVector();
	AnyCell *parseBytevector();

	AnyCell *parseDatumLabel(char firstDigit);

	World &m_world;
	std::istream &m_inStream;

	std::unordered_map<long long, alloc::StrongRef<AnyCell>> m_datumLabels;
};

}


#endif
