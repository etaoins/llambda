#include "ExternalFormDatumWriter.h"

#include <cassert>
#include <sstream>
#include <iomanip>
#include <unordered_map>

#include "binding/BoxedDatum.h"
#include "binding/UnspecificValue.h"
#include "binding/BooleanValue.h"
#include "binding/ExactIntegerValue.h"
#include "binding/InexactRationalValue.h"
#include "binding/SymbolValue.h"
#include "binding/StringValue.h"
#include "binding/PairValue.h"

namespace lliby
{

void ExternalFormDatumWriter::render(const BoxedDatum *datum)
{
	if (auto value = datum->asUnspecificValue())
	{
		renderUnspecific(value);
	}
	else if (auto value = datum->asEmptyListValue())
	{
		renderEmptyList(value);
	}
	else if (auto value = datum->asBooleanValue())
	{
		renderBoolean(value);
	}
	else if (auto value = datum->asExactIntegerValue())
	{
		renderExactInteger(value);
	}
	else if (auto value = datum->asInexactRationalValue())
	{
		renderInexactRational(value);
	}
	else if (auto value = datum->asSymbolValue())
	{
		renderStringLike(value, static_cast<std::uint8_t>('|'), false);
	}
	else if (auto value = datum->asStringValue())
	{
		renderStringLike(value, static_cast<std::uint8_t>('"'), true);
	}
	else if (auto value = datum->asPairValue())
	{
		renderPair(value);
	}
	else
	{
		assert(false);
	}
}

void ExternalFormDatumWriter::renderUnspecific(const UnspecificValue *)
{
	m_outStream << "#!unspecific";
}

void ExternalFormDatumWriter::renderEmptyList(const EmptyListValue *)
{
	m_outStream << "()";
}

void ExternalFormDatumWriter::renderBoolean(const BooleanValue *value)
{
	if (value->value())
	{
		m_outStream << "#t";
	}
	else
	{
		m_outStream << "#f";
	}
}

void ExternalFormDatumWriter::renderExactInteger(const ExactIntegerValue *value)
{
	m_outStream << value->value();
}

void ExternalFormDatumWriter::renderInexactRational(const InexactRationalValue *value)
{
	if (value->isNaN())
	{
		m_outStream << "+nan.0";
	}
	else if (value->isNegativeInfinity())
	{
		m_outStream << "-inf.0";
	}
	else if (value->isPositiveInfinity())
	{
		m_outStream << "+inf.0";
	}
	else
	{
		m_outStream << value->value();

		if (value->isInteger())
		{
			// Add on ".0" to indicate inexactness
			m_outStream << ".0";
		}
	}
}
	
void ExternalFormDatumWriter::renderStringLike(const StringLikeValue *value, std::uint8_t quoteChar, bool needsQuotes)
{
	std::ostringstream outBuf;

	const std::unordered_map<std::uint8_t, const char *> specialChars = {
		{0x07, "\\a"},
		{0x08, "\\b"},
		{0x09, "\\t"},
		{0x0a, "\\n"},
		{0x0d, "\\r"},
		{0x20, " "},
		{0x5c, "\\\\"},
		{0x22, "\""}
	};

	if (value->byteLength() == 0)
	{
		// This is for the empty symbol which is represented by ||
		needsQuotes = true;
	}

	for(std::uint32_t i = 0; i < value->byteLength(); i++)
	{
		std::uint8_t byteValue = value->utf8Data()[i];

		if (byteValue == quoteChar)
		{
			needsQuotes = true;
			outBuf << "\\" << static_cast<char>(byteValue);
		}
		else if (// Has to be above the control character and whitespace range
			(byteValue > 0x20) &&
			// Can't be a literal backslash
			(byteValue != 0x5c) && 
			// Can't be DEL
			(byteValue != 0x7f) &&
			// Can't be " even if it's not the quote character
			(byteValue != 0x22))
		{
			// These can be used literally
			outBuf << static_cast<char>(byteValue);
		}
		else
		{
			needsQuotes = true;

			auto charReplacement = specialChars.find(byteValue);

			if (charReplacement != specialChars.cend())
			{
				outBuf << charReplacement->second;
			}
			else
			{
				// These are anonymous control characters
				outBuf << "\\x" << std::hex << byteValue;
			}
		}
	}

	if (needsQuotes)
	{
		m_outStream << static_cast<char>(quoteChar) << outBuf.str() << static_cast<char>(quoteChar);
	}
	else
	{
		m_outStream << outBuf.str();
	}
}
	
void ExternalFormDatumWriter::renderPair(const PairValue *value, bool inList)
{
	if (!inList)
	{
		m_outStream << "(";
	}

	render(value->car());

	if (value->cdr()->isEmptyListValue())
	{
		m_outStream << ")";
	}
	else if (auto rest = value->cdr()->asPairValue())
	{
		m_outStream << " ";
		renderPair(rest, true);
	}
	else 
	{
		m_outStream << " . ";
		render(value->cdr());
		m_outStream << ")";
	}
}

}
