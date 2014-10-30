#include "ExternalFormDatumWriter.h"

#include <cassert>
#include <iomanip>

#include "binding/AnyCell.h"
#include "binding/UnitCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/SymbolCell.h"
#include "binding/StringCell.h"
#include "binding/PairCell.h"
#include "binding/BytevectorCell.h"
#include "binding/VectorCell.h"
#include "binding/CharCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ProcedureCell.h"
#include "binding/RecordCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/PortCell.h"

#include "dynamic/ParameterProcedureCell.h"
#include "dynamic/EscapeProcedureCell.h"

namespace
{
	bool stringLikeByteIsDirectlyPrintable(std::uint8_t byteValue)
	{
		return
			// Has to be above the control character and whitespace range
			(byteValue > 0x20) &&
			// Can't be a literal backslash
			(byteValue != 0x5c) &&
			// Can't be DEL
			(byteValue != 0x7f) &&
			// Can't be " even if it's not the quote character
			(byteValue != 0x22);
	}
}

namespace lliby
{

void ExternalFormDatumWriter::render(const AnyCell *datum)
{
	if (auto value = cell_cast<UnitCell>(datum))
	{
		renderUnit(value);
	}
	else if (auto value = cell_cast<EmptyListCell>(datum))
	{
		renderEmptyList(value);
	}
	else if (auto value = cell_cast<BooleanCell>(datum))
	{
		renderBoolean(value);
	}
	else if (auto value = cell_cast<ExactIntegerCell>(datum))
	{
		renderExactInteger(value);
	}
	else if (auto value = cell_cast<FlonumCell>(datum))
	{
		renderFlonum(value);
	}
	else if (auto value = cell_cast<SymbolCell>(datum))
	{
		renderStringLike(value->constUtf8Data(), value->byteLength(), static_cast<std::uint8_t>('|'), false);
	}
	else if (auto value = cell_cast<StringCell>(datum))
	{
		renderStringLike(value->constUtf8Data(), value->byteLength(), static_cast<std::uint8_t>('"'), true);
	}
	else if (auto value = cell_cast<PairCell>(datum))
	{
		renderPair(value);
	}
	else if (auto value = cell_cast<BytevectorCell>(datum))
	{
		renderBytevector(value);
	}
	else if (auto value = cell_cast<VectorCell>(datum))
	{
		renderVector(value);
	}
	else if (auto value = cell_cast<ProcedureCell>(datum))
	{
		renderProcedure(value);
	}
	else if (auto value = cell_cast<CharCell>(datum))
	{
		renderCharacter(value);
	}
	else if (auto value = cell_cast<RecordCell>(datum))
	{
		renderRecord(value);
	}
	else if (auto errorObj = cell_cast<ErrorObjectCell>(datum))
	{
		renderErrorObject(errorObj);
	}
	else if (auto port = cell_cast<PortCell>(datum))
	{
		renderPort(port);
	}
	else
	{
		assert(false);
	}
}

void ExternalFormDatumWriter::renderUnit(const UnitCell *)
{
	m_outStream << "#!unit";
}

void ExternalFormDatumWriter::renderEmptyList(const EmptyListCell *)
{
	m_outStream << "()";
}

void ExternalFormDatumWriter::renderBoolean(const BooleanCell *value)
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

void ExternalFormDatumWriter::renderExactInteger(const ExactIntegerCell *value)
{
	m_outStream << value->value();
}

void ExternalFormDatumWriter::renderFlonum(const FlonumCell *value)
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
		m_outStream << std::setprecision(256) << value->value();

		if (value->isInteger())
		{
			// Add on ".0" to indicate inexactness
			m_outStream << ".0";
		}
	}
}

void ExternalFormDatumWriter::renderStringLike(const std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint8_t quoteChar, bool needsQuotes)
{
	if (byteLength == 0)
	{
		// This is for the empty symbol which is represented by ||
		needsQuotes = true;
	}
	else if (!needsQuotes)
	{
		// Scan the string to determine if we need quotes
		for(std::uint32_t i = 0; i < byteLength; i++)
		{
			const std::uint8_t byteValue = utf8Data[i];

			if ((byteValue == quoteChar) || (byteValue > 0x7f) || !stringLikeByteIsDirectlyPrintable(byteValue))
			{
				needsQuotes = true;
				break;
			}
		}
	}

	if (!needsQuotes)
	{
		// We can print this directly without any transformation
		m_outStream.write(reinterpret_cast<const char *>(utf8Data), byteLength);
		return;
	}

	m_outStream << static_cast<char>(quoteChar);

	for(std::uint32_t i = 0; i < byteLength; i++)
	{
		const std::uint8_t byteValue = utf8Data[i];

		if (byteValue == quoteChar)
		{
			m_outStream << "\\" << static_cast<char>(byteValue);
		}
		else if (stringLikeByteIsDirectlyPrintable(byteValue))
		{
			m_outStream << static_cast<char>(byteValue);
		}
		else
		{
			switch(byteValue)
			{
			case 0x07: m_outStream << "\\a";  break;
			case 0x08: m_outStream << "\\b";  break;
			case 0x09: m_outStream << "\\t";  break;
			case 0x0a: m_outStream << "\\n";  break;
			case 0x0d: m_outStream << "\\r";  break;
			case 0x20: m_outStream << " ";    break;
			case 0x5c: m_outStream << "\\\\"; break;
			case 0x22: m_outStream << "\"";   break;
			default:
				m_outStream << "\\x" << std::hex << static_cast<int>(byteValue) << ";";
			}
		}
	}

	m_outStream << static_cast<char>(quoteChar);
}

void ExternalFormDatumWriter::renderPair(const PairCell *value, bool inList)
{
renderPairEntry:
	if (!inList)
	{
		m_outStream << '(';
	}

	render(value->car());

	if (EmptyListCell::isInstance(value->cdr()))
	{
		m_outStream << ')';
	}
	else if (auto rest = cell_cast<PairCell>(value->cdr()))
	{
		m_outStream << ' ';

		// Force tail recursion here for the cdr so we can render deep lists
		value = rest;
		inList = true;
		goto renderPairEntry;
	}
	else
	{
		m_outStream << " . ";
		render(value->cdr());
		m_outStream << ')';
	}
}
	
void ExternalFormDatumWriter::renderBytevector(const BytevectorCell *value)
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

void ExternalFormDatumWriter::renderVector(const VectorCell *value)
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

void ExternalFormDatumWriter::renderProcedure(const ProcedureCell *proc)
{
	if (proc->capturesVariables())
	{
		if (dynamic::ParameterProcedureCell::isInstance(proc))
		{
			m_outStream << "#!procedure(parameter:" << proc << ")";
		}
		else if (dynamic::EscapeProcedureCell::isInstance(proc))
		{
			m_outStream << "#!procedure(escape:" << proc << ")";
		}
		else
		{
			m_outStream << "#!procedure(closure:" << proc << "/" << reinterpret_cast<void*>(proc->entryPoint()) << ")";
		}
	}
	else
	{
		m_outStream << "#!procedure(emptyclosure/" << reinterpret_cast<void*>(proc->entryPoint()) << ")";
	}

}

void ExternalFormDatumWriter::renderCharacter(const CharCell *value)
{
	std::int32_t codePoint = value->unicodeChar().codePoint();

	if ((codePoint >= 0x21) && (codePoint <= 0x7e))
	{
		m_outStream << "#\\" << static_cast<char>(codePoint);
	}
	else
	{
		switch(codePoint)
		{
		case 0x07: m_outStream << "#\\alarm";     break;
		case 0x08: m_outStream << "#\\backspace"; break;
		case 0x7f: m_outStream << "#\\delete";    break;
		case 0x1b: m_outStream << "#\\escape";    break;
		case 0x0a: m_outStream << "#\\newline";   break;
		case 0x00: m_outStream << "#\\null";      break;
		case 0x0d: m_outStream << "#\\return";    break;
		case 0x20: m_outStream << "#\\space";     break;
		case 0x09: m_outStream << "#\\tab";       break;
		default:
			m_outStream << "#\\x" << std::hex << codePoint;
		}
	}

}

void ExternalFormDatumWriter::renderRecord(const RecordCell *)
{
	// XXX: Can codegen give us enough type information to render record contents?
	m_outStream << "#!record";
}

void ExternalFormDatumWriter::renderErrorObject(const ErrorObjectCell *errObj)
{
	m_outStream << "#!error(" << errObj->message() << ")";
}
	
void ExternalFormDatumWriter::renderPort(const PortCell *value)
{
	m_outStream << "#!port";
}

}
