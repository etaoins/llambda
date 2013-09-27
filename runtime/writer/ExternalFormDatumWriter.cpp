#include "ExternalFormDatumWriter.h"

#include <cassert>
#include <sstream>
#include <iomanip>
#include <unordered_map>

#include "binding/BoxedDatum.h"
#include "binding/BoxedUnspecific.h"
#include "binding/BoxedBoolean.h"
#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"
#include "binding/BoxedSymbol.h"
#include "binding/BoxedString.h"
#include "binding/BoxedPair.h"
#include "binding/BoxedByteVector.h"
#include "binding/BoxedVector.h"
#include "binding/BoxedCharacter.h"

namespace lliby
{

void ExternalFormDatumWriter::render(const BoxedDatum *datum)
{
	if (auto value = datum->asBoxedUnspecific())
	{
		renderUnspecific(value);
	}
	else if (auto value = datum->asBoxedEmptyList())
	{
		renderEmptyList(value);
	}
	else if (auto value = datum->asBoxedBoolean())
	{
		renderBoolean(value);
	}
	else if (auto value = datum->asBoxedExactInteger())
	{
		renderExactInteger(value);
	}
	else if (auto value = datum->asBoxedInexactRational())
	{
		renderInexactRational(value);
	}
	else if (auto value = datum->asBoxedSymbol())
	{
		renderStringLike(value, static_cast<std::uint8_t>('|'), false);
	}
	else if (auto value = datum->asBoxedString())
	{
		renderStringLike(value, static_cast<std::uint8_t>('"'), true);
	}
	else if (auto value = datum->asBoxedPair())
	{
		renderPair(value);
	}
	else if (auto value = datum->asBoxedByteVector())
	{
		renderByteVector(value);
	}
	else if (auto value = datum->asBoxedVector())
	{
		renderVector(value);
	}
	else if (auto value = datum->asBoxedProcedure())
	{
		renderProcedure(value);
	}
	else if (auto value = datum->asBoxedCharacter())
	{
		renderCharacter(value);
	}
	else
	{
		assert(false);
	}
}

void ExternalFormDatumWriter::renderUnspecific(const BoxedUnspecific *)
{
	m_outStream << "#!unspecific";
}

void ExternalFormDatumWriter::renderEmptyList(const BoxedEmptyList *)
{
	m_outStream << "()";
}

void ExternalFormDatumWriter::renderBoolean(const BoxedBoolean *value)
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

void ExternalFormDatumWriter::renderExactInteger(const BoxedExactInteger *value)
{
	m_outStream << value->value();
}

void ExternalFormDatumWriter::renderInexactRational(const BoxedInexactRational *value)
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
	
void ExternalFormDatumWriter::renderStringLike(const BoxedStringLike *value, std::uint8_t quoteChar, bool needsQuotes)
{
	std::ostringstream outBuf;

	static const std::unordered_map<std::uint8_t, const char *> specialChars = {
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
			if (byteValue > 0x7f)
			{
				// R7RS requires symbols to be quoted if they have non-ASCII characters
				needsQuotes = true;
			}

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
	
void ExternalFormDatumWriter::renderPair(const BoxedPair *value, bool inList)
{
	if (!inList)
	{
		m_outStream << "(";
	}

	render(value->car());

	if (value->cdr()->isBoxedEmptyList())
	{
		m_outStream << ")";
	}
	else if (auto rest = value->cdr()->asBoxedPair())
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
	
void ExternalFormDatumWriter::renderByteVector(const BoxedByteVector *value)
{
	bool printedByte = false;
	m_outStream << "#u8(";

	for(unsigned int i = 0; i < value->length(); i++)
	{
		if (printedByte)
		{
			// Pad with a space
			m_outStream << " ";
		}

		m_outStream << value->byteAt(i);

		printedByte = true;
	}

	m_outStream << ")";
}

void ExternalFormDatumWriter::renderVector(const BoxedVector *value)
{
	bool printedElement = false;
	m_outStream << "#(";

	for(unsigned int i = 0; i < value->length(); i++)
	{
		if (printedElement)
		{
			// Pad with a space
			m_outStream << " ";
		}

		render(value->elementAt(i));

		printedElement = true;
	}

	m_outStream << ")";
}

void ExternalFormDatumWriter::renderProcedure(const BoxedProcedure *)
{
	m_outStream << "#!procedure";
}

void ExternalFormDatumWriter::renderCharacter(const BoxedCharacter *value)
{
	std::int32_t codePoint = value->unicodeChar().codePoint();
	static const std::unordered_map<std::uint32_t, const char *> specialChars = {
		{0x07, "#\\alarm"},
		{0x08, "#\\backspace"},
		{0x7f, "#\\delete"},
		{0x1b, "#\\escape"},
		{0x0a, "#\\newline"},
		{0x00, "#\\null"},
		{0x0d, "#\\return"},
		{0x20, "#\\space"},
		{0x09, "#\\tab"},
	};

	if ((codePoint >= 0x21) && (codePoint <= 0x7e))
	{
		m_outStream << "#\\" << static_cast<char>(codePoint);
	}
	else
	{
		auto specialCharIt = specialChars.find(codePoint);

		if (specialCharIt != specialChars.cend())
		{
			m_outStream << specialCharIt->second;
		}
		else
		{
			m_outStream << "#\\x" << std::hex << codePoint;
		}
	}

}

}
