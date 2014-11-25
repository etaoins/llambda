#include "DisplayDatumWriter.h"

#include <sstream>

#include "binding/CharCell.h"
#include "unicode/utf8.h"

namespace lliby
{

void DisplayDatumWriter::renderStringLike(const std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint8_t quoteChar, bool needsQuotes)
{
	// Display completely unquoted
	m_outStream.write(reinterpret_cast<const char *>(utf8Data), byteLength);
}

void DisplayDatumWriter::renderCharacter(const CharCell *value)
{
	// Write the raw UTF-8 value
	utf8::EncodedChar utf8Bytes(utf8::encodeChar(value->unicodeChar()));
	m_outStream.write(reinterpret_cast<const char *>(utf8Bytes.data), utf8Bytes.size);
}

}
