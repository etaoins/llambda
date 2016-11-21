#include "ExternalFormDatumWriter.h"

#include <cassert>
#include <strings.h>
#include <iomanip>
#include <cmath>

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
#include "binding/EofObjectCell.h"
#include "binding/MailboxCell.h"
#include "binding/HashMapCell.h"

#include "dynamic/ParameterProcedureCell.h"

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

	bool byteIsSymbolInitial(std::uint8_t c)
	{
		// Letter?
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
		{
			return true;
		}

		return (c == '!' || c == '$' || c == '%' || c == '&' || c == '*' || c == '/' || c == ':' || c == '<' ||
				c == '=' || c == '>' || c == '?' || c == '^' || c == '_' || c == '~');
	}

	bool byteIsSymbolSubsequent(std::uint8_t c)
	{
		if (c >= 0x7f)
		{
			// Non-ASCII
			return false;
		}

		if (c == '|' || c == '"' || c == '[' || c == ']' || c == '(' || c == ')' || c == '#' || c == '\'' ||
		    c == '`' || c == ',')
		{
			// Syntax character
			return false;
		}

		return stringLikeByteIsDirectlyPrintable(c);
	}

	bool byteIsExplicitSign(std::uint8_t c)
	{
		return (c == '+') || (c == '-');
	}

	bool byteIsSignSubsequent(std::uint8_t c)
	{
		return byteIsExplicitSign(c) || byteIsSymbolInitial(c) || (c == '@');
	}

	bool byteIsDotSubsequent(std::uint8_t c)
	{
		return byteIsSignSubsequent(c) || (c == '.');
	}

	bool strIsInfNan(const std::uint8_t *utf8Data, std::uint32_t byteLength)
	{
		return (byteLength >= 6) && byteIsExplicitSign(utf8Data[0]) &&
		       (!strncasecmp(reinterpret_cast<const char *>(&utf8Data[1]), "inf.0", 5) ||
		        !strncasecmp(reinterpret_cast<const char *>(&utf8Data[1]), "nan.0", 5));
	}

	bool strIsPeculiarIdentifier(const std::uint8_t *utf8Data, std::uint32_t byteLength)
	{
		if (strIsInfNan(utf8Data, byteLength))
		{
			return false;
		}

		if (byteLength == 1)
		{
			return byteIsExplicitSign(utf8Data[0]);
		}

		assert(byteLength > 1);

		if (byteIsExplicitSign(utf8Data[0]))
		{
			if (byteIsSignSubsequent(utf8Data[1]))
			{
				for(std::uint32_t i = 2; i < byteLength; i++)
				{
					if (!byteIsSymbolSubsequent(utf8Data[i]))
					{
						return false;
					}
				}

				return true;
			}
			else if ((utf8Data[1] == '.') && (byteLength > 2))
			{
				// Shift off the explicit sign
				return strIsPeculiarIdentifier(&utf8Data[1], byteLength - 1);
			}
		}
		else if ((utf8Data[0] == '.') && byteIsDotSubsequent(utf8Data[1]))
		{
			for(std::uint32_t i = 2; i < byteLength; i++)
			{
				if (!byteIsSymbolSubsequent(utf8Data[i]))
				{
					return false;
				}
			}

			return true;
		}

		return false;
	}

	bool strIsIdentifier(const std::uint8_t *utf8Data, std::uint32_t byteLength)
	{
		if (byteLength == 0)
		{
			// Empty identifier
			return false;
		}

		// Scan the symbol to determine if we need quotes
		if (!byteIsSymbolInitial(utf8Data[0]))
		{
			return strIsPeculiarIdentifier(utf8Data, byteLength);
		}

		for(std::uint32_t i = 1; i < byteLength; i++)
		{
			if (!byteIsSymbolSubsequent(utf8Data[i]))
			{
				// Not a symbol subsequent
				return false;
			}
		}

		return true;
	}
}

namespace lliby
{

void ExternalFormDatumWriter::render(const AnyCell *datum, int defaultRadix)
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
		renderExactInteger(value, defaultRadix);
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
	else if (auto eofObj = cell_cast<EofObjectCell>(datum))
	{
		renderEofObject(eofObj);
	}
	else if (auto value = cell_cast<MailboxCell>(datum))
	{
		renderMailbox(value);
	}
	else if (auto value = cell_cast<HashMapCell>(datum))
	{
		renderHashMap(value);
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

void ExternalFormDatumWriter::renderExactInteger(const ExactIntegerCell *value, int defaultRadix)
{
	// Non-decimal bases don't work with negative numbers
	const std::int64_t signedNumber = value->value();
	bool negative = (signedNumber < 0);
	std::uint64_t absoluteNumber = negative ? -signedNumber : signedNumber;

	switch(defaultRadix)
	{
	case 2:
		{
			// Space for 64bit + #b + sign + NULL terminator
			char outputBuffer[68];
			char *outPtr = &outputBuffer[sizeof(outputBuffer)];

			*(--outPtr) = 0;

			if (absoluteNumber == 0)
			{
				// Need at least one number
				*(--outPtr) = '0';
			}
			else
			{
				while(absoluteNumber)
				{
					*(--outPtr) = (absoluteNumber & 0x1) ? '1' : '0';
					absoluteNumber = absoluteNumber >> 1;
				}

				if (negative)
				{
					*(--outPtr) = '-';
				}
			}

			*(--outPtr) = 'b';
			*(--outPtr) = '#';

			m_outStream << outPtr;
		}
		break;

	case 8:
		m_outStream << "#o";

		if (negative)
		{
			m_outStream << "-";
		}

		m_outStream << std::oct << absoluteNumber;
		break;
	case 16:
		m_outStream << "#x";

		if (negative)
		{
			m_outStream << "-";
		}

		m_outStream << std::hex << absoluteNumber;
		break;
	default:
		if (negative)
		{
			m_outStream << "-";
		}

		m_outStream << std::dec << absoluteNumber;
		break;
	}
}

void ExternalFormDatumWriter::renderFlonum(const FlonumCell *value)
{
	const double number = value->value();

	if (std::isnan(number))
	{
		m_outStream << "+nan.0";
	}
	else if (std::isinf(number))
	{
		if (number < 0.0)
		{
			m_outStream << "-inf.0";
		}
		else
		{
			m_outStream << "+inf.0";
		}
	}
	else
	{
		m_outStream << std::setprecision(1024) << number;

		double unused;
		if (std::modf(number, &unused) == 0.0)
		{
			// Add on ".0" to indicate inexactness
			m_outStream << ".0";
		}
	}
}

void ExternalFormDatumWriter::renderStringLike(const std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint8_t quoteChar, bool needsQuotes)
{
	if (!needsQuotes)
	{
		needsQuotes = !strIsIdentifier(utf8Data, byteLength);
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

	for(BytevectorCell::LengthType i = 0; i < value->length(); i++)
	{
		if (printedByte)
		{
			// Pad with a space
			m_outStream << " ";
		}

		m_outStream << std::dec << value->byteAt(i);

		printedByte = true;
	}

	m_outStream << ")";
}

void ExternalFormDatumWriter::renderVector(const VectorCell *value)
{
	bool printedElement = false;
	m_outStream << "#(";

	for(VectorCell::LengthType i = 0; i < value->length(); i++)
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
	m_outStream << "#!error(";

	if (errObj->category() != ErrorCategory::Default)
	{
		m_outStream << schemeNameForErrorCategory(errObj->category()) << "/";
	}

	m_outStream << errObj->message() << ")";
}

void ExternalFormDatumWriter::renderPort(const PortCell *value)
{
	m_outStream << "#!port";
}

void ExternalFormDatumWriter::renderEofObject(const EofObjectCell *value)
{
	m_outStream << "#!eof";
}

void ExternalFormDatumWriter::renderMailbox(const MailboxCell *value)
{
	m_outStream << "#!mailbox";
}

void ExternalFormDatumWriter::renderHashMap(const HashMapCell *value)
{
	m_outStream << "#!hash-map";
}

}
