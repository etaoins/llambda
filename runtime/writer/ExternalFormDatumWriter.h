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
	virtual void renderUnspecific(const UnspecificValue *value);
	virtual void renderEmptyList(const EmptyListValue *value);
	virtual void renderBoolean(const BooleanValue *value);
	virtual void renderExactInteger(const ExactIntegerValue *value);
	virtual void renderInexactRational(const InexactRationalValue *value);
	virtual void renderStringLike(const StringLikeValue *value, std::uint8_t quoteChar, bool needsQuotes);
	virtual void renderPair(const PairValue *value, bool inList = false);
	virtual void renderByteVector(const ByteVectorValue *value);
	virtual void renderVector(const VectorValue *value);
	virtual void renderProcedure(const ProcedureValue *value);

	std::ostream &m_outStream;
};

}

#endif
