#ifndef _LLIBY_WRITER_DISPLAYDATUMWRITER_H
#define _LLIBY_WRITER_DISPLAYDATUMWRITER_H

#include "ExternalFormDatumWriter.h"

namespace lliby
{

class DisplayDatumWriter : public ExternalFormDatumWriter
{
public:
	explicit DisplayDatumWriter(std::ostream &outStream) :
		ExternalFormDatumWriter(outStream)
	{
	}

protected:
	virtual void renderStringLike(const std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint8_t quoteChar, bool needsQuotes) override;
	virtual void renderCharacter(const CharCell *value) override;
};

}

#endif
