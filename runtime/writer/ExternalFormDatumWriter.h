#ifndef _LLIBY_WRITER_EXTERNALFORMDATUMWRITER_H
#define _LLIBY_WRITER_EXTERNALFORMDATUMWRITER_H

#include <ostream>

#include "DatumWriter.h"

namespace lliby
{

class ExternalFormDatumWriter : public DatumWriter
{
public:
	explicit ExternalFormDatumWriter(std::ostream &outStream) :
		m_outStream(outStream)
	{
	}

	virtual void render(const BoxedDatum *datum);

protected:
	virtual void renderUnspecific(const BoxedUnspecific *value);
	virtual void renderEmptyList(const BoxedEmptyList *value);
	virtual void renderBoolean(const BoxedBoolean *value);
	virtual void renderExactInteger(const BoxedExactInteger *value);
	virtual void renderInexactRational(const BoxedInexactRational *value);
	virtual void renderStringLike(const BoxedStringLike *value, std::uint8_t quoteChar, bool needsQuotes);
	virtual void renderPair(const BoxedPair *value, bool inList = false);
	virtual void renderBytevector(const BoxedBytevector *value);
	virtual void renderVector(const BoxedVector *value);
	virtual void renderProcedure(const BoxedProcedure *value);
	virtual void renderCharacter(const BoxedCharacter *value);

	std::ostream &m_outStream;
};

}

#endif
