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

	virtual void render(const AnyCell *datum, int defaultRadix = 10);

protected:
	virtual void renderUnit(const UnitCell *value);
	virtual void renderEmptyList(const EmptyListCell *value);
	virtual void renderBoolean(const BooleanCell *value);
	virtual void renderExactInteger(const ExactIntegerCell *value, int defaultRadix = 10);
	virtual void renderFlonum(const FlonumCell *value);
	virtual void renderStringLike(const std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint8_t quoteChar, bool needsQuotes);
	virtual void renderPair(const PairCell *value, bool inList = false);
	virtual void renderBytevector(const BytevectorCell *value);
	virtual void renderVector(const VectorCell *value);
	virtual void renderProcedure(const ProcedureCell *value);
	virtual void renderCharacter(const CharCell *value);
	virtual void renderRecord(const RecordCell *value);
	virtual void renderErrorObject(const ErrorObjectCell *value);
	virtual void renderPort(const PortCell *value);
	virtual void renderEofObject(const EofObjectCell *value);

	std::ostream &m_outStream;
};

}

#endif
