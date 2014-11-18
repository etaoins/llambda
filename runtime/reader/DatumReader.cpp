#include "DatumReader.h"
#include "ReadErrorException.h"
#include "ParserHelpers.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <ctype.h>
#include <iterator>

#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/EofObjectCell.h"
#include "binding/BooleanCell.h"
#include "binding/SymbolCell.h"
#include "binding/StringCell.h"
#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/VectorCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharCell.h"
#include "binding/ProperList.h"

#include "alloc/cellref.h"
#include "alloc/StrongRefVector.h"

#include "unicode/utf8.h"
#include "unicode/utf8/InvalidByteSequenceException.h"

namespace lliby
{

namespace
{
	int inputOffset(std::streambuf *rdbuf)
	{
		return rdbuf->pubseekoff(0, std::ios::cur, std::ios::in);
	}

	bool isIdentifierChar(char c)
	{
		return
			// Has to be above the control character and whitespace range
			(c > 0x20) &&
			// Can't be a literal backslash
			(c != 0x5c) &&
			// Can't be any syntax characters
			(c != '|') && (c != '"') && (c != '[') && (c != ']') && (c != '(') && (c != ')') && (c != '#') &&
			(c != '\'') && (c != '`') && (c != ',') &&
			// Can't be DEL or above
			(c < 0x7f);
	}

	std::string takeQuotedStringLike(std::streambuf *rdbuf, char quoteChar)
	{
		std::string accum;

		while(true)
		{
			int nextChar = rdbuf->sbumpc();

			if (nextChar == EOF)
			{
				// Out of data without closing quote
				throw UnexpectedEofException(inputOffset(rdbuf), "End of input without closing quote for string-like");
			}

			if (nextChar == quoteChar)
			{
				return accum;
			}
			else if (nextChar == '\\')
			{
				// This is a quoted character
				nextChar = rdbuf->sbumpc();

				if (nextChar == EOF)
				{
					// Out of data without closing quote
					throw UnexpectedEofException(inputOffset(rdbuf), "End of input during backslash escaped sequence");
				}

				switch(nextChar)
				{
				case '\\': accum.push_back('\\'); break;
				case 'a':  accum.push_back(0x07); break;
				case 'b':  accum.push_back(0x08); break;
				case 't':  accum.push_back(0x09); break;
				case 'n':  accum.push_back(0x0a); break;
				case 'r':  accum.push_back(0x0d); break;
				case '"':  accum.push_back(0x22); break;
				case '|':  accum.push_back(0x7c); break;
				case 'x':
					{
						// Hex escape
						std::string hexCode = takeHexadecimal(rdbuf);

						nextChar = rdbuf->sbumpc();

						if (nextChar != ';')
						{
							throw MalformedDatumException(inputOffset(rdbuf), "Hex escape not terminated with ;");
						}
						else if (hexCode.empty())
						{
							throw MalformedDatumException(inputOffset(rdbuf), "Empty hex escape");
						}

						UnicodeChar escapedChar(strtol(hexCode.c_str(), nullptr, 16));
						utf8::appendChar(escapedChar, std::back_inserter(accum));
					}
					break;

				case '\n':
					// Discard the intraline whitespace at the beginning of the next line
					discardWhile(rdbuf, [] (char c)
					{
						return (c == ' ') || (c == '\t');
					});

					break;

				default:   accum.push_back('\\'); accum.push_back(nextChar);
				}
			}
			else
			{
				accum.push_back(nextChar);
			}
		}
	}
}

AnyCell* DatumReader::parse(int defaultRadix)
{
	std::istream::sentry sen(m_inStream, true);

	if (!sen)
	{
		return EofObjectCell::instance();
	}

	try
	{
		AnyCell *result = parseDatum(defaultRadix);

		if (result == EofObjectCell::instance())
		{
			m_inStream.setstate(std::ios::eofbit);
		}

		return result;
	}
	catch(UnexpectedEofException)
	{
		m_inStream.setstate(std::ios::eofbit);
		throw;
	}
}

AnyCell* DatumReader::parseDatum(int defaultRadix)
{
	consumeWhitespace();

	int peekChar = rdbuf()->sgetc();

	if (peekChar == EOF)
	{
		return EofObjectCell::instance();
	}
	else if ((peekChar >= '0') && (peekChar <= '9'))
	{
		return parseUnradixedNumber(defaultRadix);
	}
	else if (peekChar == '.')
	{
		try
		{
			return parseUnradixedNumber(defaultRadix);
		}
		catch(ReadErrorException)
		{
			return parseSymbol();
		}
	}
	else if (peekChar == '+')
	{
		try
		{
			return parsePositiveNumber(defaultRadix);
		}
		catch(ReadErrorException)
		{
			return parseSymbol();
		}
	}
	else if (peekChar == '-')
	{
		try
		{
			return parseNegativeNumber(defaultRadix);
		}
		catch(ReadErrorException)
		{
			return parseSymbol();
		}
	}
	else if (peekChar == '#')
	{
		return parseOctoDatum();
	}
	else if (peekChar == '|')
	{
		return parseEnclosedSymbol();
	}
	else if (peekChar == '"')
	{
		return parseString();
	}
	else if (peekChar == '(')
	{
		return parseList(')');
	}
	else if (peekChar == '[')
	{
		return parseList(']');
	}
	else if (peekChar == '\'')
	{
		return parseSymbolShorthand("quote");
	}
	else if (peekChar == '`')
	{
		return parseSymbolShorthand("quasiquote");
	}
	else if (peekChar == ',')
	{
		if (rdbuf()->snextc() == '@')
		{
			return parseSymbolShorthand("unquote-splicing");
		}
		else
		{
			rdbuf()->sputbackc(',');
			return parseSymbolShorthand("unquote");
		}
	}
	else
	{
		// Everything else is a symbol
		return parseSymbol();
	}
}

int DatumReader::consumeWhitespace()
{
	while(true)
	{
		int peekChar = rdbuf()->sgetc();

		if ((peekChar == '\r') || (peekChar == '\n') || (peekChar == '\t') || (peekChar == ' '))
		{
			rdbuf()->sbumpc();
		}
		else if (peekChar == ';')
		{
			// Consume until the end of the line
			int getChar;
			do
			{
				getChar = rdbuf()->sbumpc();
			}
			while((getChar != '\n') && (getChar != EOF));
		}
		else if (peekChar == '#')
		{
			// This could be one of the R7RS comment types
			rdbuf()->sbumpc();

			peekChar = rdbuf()->sgetc();
			if (peekChar == ';')
			{
				// Discard the commented out datum
				rdbuf()->sbumpc();
				consumeWhitespace();
				parse();
			}
			else if (peekChar == '|')
			{
				rdbuf()->sbumpc();
				consumeBlockComment();
				consumeWhitespace();
			}
			else
			{
				rdbuf()->sputbackc('#');
			}

			return peekChar;
		}
		else
		{
			// All done
			return peekChar;
		}
	}
}

void DatumReader::consumeBlockComment()
{
	int commentDepth = 1;

	while(true)
	{
		int firstChar = rdbuf()->sbumpc();

		if (firstChar == EOF)
		{
			return;
		}
		else if (firstChar == '#')
		{
			if (rdbuf()->sbumpc() == '|')
			{
				++commentDepth;
			}
		}
		else if (firstChar == '|')
		{
			if (rdbuf()->sbumpc() == '#')
			{
				if (--commentDepth == 0)
				{
					return;
				}
			}
		}
	}
}

AnyCell* DatumReader::parseOctoDatum()
{
	// Consume the #
	rdbuf()->sbumpc();

	int getChar = rdbuf()->sbumpc();

	if ((getChar == 'b') || (getChar == 'B'))
	{
		return parseNumber(2);
	}
	else if ((getChar == 'o') || (getChar == 'O'))
	{
		return parseNumber(8);
	}
	else if ((getChar == 'd') || (getChar == 'D'))
	{
		return parseNumber(10);
	}
	else if ((getChar == 'x') || (getChar == 'X'))
	{
		return parseNumber(16);
	}
	else if (getChar == 't')
	{
		consumeLiteral(rdbuf(), "rue");
		return BooleanCell::trueInstance();
	}
	else if (getChar == 'f')
	{
		consumeLiteral(rdbuf(), "alse");
		return BooleanCell::falseInstance();
	}
	else if (getChar == '(')
	{
		return parseVector();
	}
	else if (getChar == '!')
	{
		if (consumeLiteral(rdbuf(), "unit"))
		{
			return UnitCell::instance();
		}
	}
	else if (getChar == 'u')
	{
		// This is the rest of #u8(, not just a sad face
		if (consumeLiteral(rdbuf(), "8("))
		{
			return parseBytevector();
		}
	}
	else if (getChar == '\\')
	{
		return parseChar();
	}
	else if ((getChar >= '0') && (getChar <= '9'))
	{
		return parseDatumLabel(getChar);
	}
	else if (getChar == EOF)
	{
		throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while parsing # datum");
	}

	throw MalformedDatumException(inputOffset(rdbuf()), "Unrecognized # datum");
}

AnyCell* DatumReader::parseEnclosedSymbol()
{
	// Consume the |
	rdbuf()->sbumpc();

	return SymbolCell::fromUtf8StdString(m_world, takeQuotedStringLike(rdbuf(), '|'));
}

AnyCell* DatumReader::parseString()
{
	// Consume the "
	rdbuf()->sbumpc();

	return StringCell::fromUtf8StdString(m_world, takeQuotedStringLike(rdbuf(), '"'));
}

AnyCell* DatumReader::parseSymbol()
{
	std::string symbolData;

	takeWhile(rdbuf(), symbolData, isIdentifierChar);

	if (symbolData.empty())
	{
		throw MalformedDatumException(inputOffset(rdbuf()), "Unrecognized start character");
	}
	else if (symbolData == ".")
	{
		throw MalformedDatumException(inputOffset(rdbuf()), ". reserved for terminating improper lists");
	}

	return SymbolCell::fromUtf8StdString(m_world, symbolData);
}

AnyCell* DatumReader::parseSymbolShorthand(const std::string &expanded)
{
	// Consume the shorthand
	rdbuf()->sbumpc();

	alloc::SymbolRef expandedSymbol(m_world, SymbolCell::fromUtf8StdString(m_world, expanded));
	AnyCell *innerDatum = parse();

	return ProperList<AnyCell>::create(m_world, {expandedSymbol, innerDatum});
}

AnyCell* DatumReader::parseChar()
{
	int nextChar = rdbuf()->sbumpc();

	if (nextChar == EOF)
	{
		throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while reading character");
	}
	else if ((nextChar == 'a') && consumeLiteral(rdbuf(), "larm"))
	{
		return CharCell::createInstance(m_world, 0x07);
	}
	else if ((nextChar == 'b') && consumeLiteral(rdbuf(), "ackspace"))
	{
		return CharCell::createInstance(m_world, 0x08);
	}
	else if ((nextChar == 'd') && consumeLiteral(rdbuf(), "elete"))
	{
		return CharCell::createInstance(m_world, 0x7f);
	}
	else if ((nextChar == 'e') && consumeLiteral(rdbuf(), "scape"))
	{
		return CharCell::createInstance(m_world, 0x1b);
	}
	else if ((nextChar == 'n') && consumeLiteral(rdbuf(), "ewline"))
	{
		return CharCell::createInstance(m_world, 0x0a);
	}
	else if ((nextChar == 'n') && consumeLiteral(rdbuf(), "ull"))
	{
		return CharCell::createInstance(m_world, 0x00);
	}
	else if ((nextChar == 'r') && consumeLiteral(rdbuf(), "eturn"))
	{
		return CharCell::createInstance(m_world, 0x0d);
	}
	else if ((nextChar == 's') && consumeLiteral(rdbuf(), "pace"))
	{
		return CharCell::createInstance(m_world, 0x20);
	}
	else if ((nextChar == 't') && consumeLiteral(rdbuf(), "ab"))
	{
		return CharCell::createInstance(m_world, 0x09);
	}
	else if ((nextChar == 'x') || (nextChar == 'X'))
	{
		std::string hexCode = takeHexadecimal(rdbuf());

		if (!hexCode.empty())
		{
			return CharCell::createInstance(m_world, strtoll(hexCode.c_str(), nullptr, 16));
		}
	}

	// Literal character - we need to parse as UTF-8
	int seqBytes = utf8::bytesInSequence(nextChar);

	if (seqBytes == -1)
	{
		throw utf8::InvalidHeaderByteException(0, 0);
	}

	std::uint8_t byteBuffer[4];
	byteBuffer[0] = nextChar;

	const int bytesToRead = seqBytes - 1;
	if (rdbuf()->sgetn(reinterpret_cast<char*>(&byteBuffer[1]), bytesToRead) != bytesToRead)
	{
		throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while reading character");
	}

	utf8::validateData(byteBuffer, &byteBuffer[seqBytes]);

	const std::uint8_t *scanPtr = byteBuffer;
	UnicodeChar parsedChar = utf8::decodeChar(&scanPtr);

	if ((parsedChar.codePoint() < '0') || ((parsedChar.codePoint() > '9')))
	{
		// If this is a non-digit then it can't be followed by an identifier character
		if (isIdentifierChar(rdbuf()->sgetc()))
		{
			throw MalformedDatumException(inputOffset(rdbuf()), "Unrecognized character name");
		}
	}

	return CharCell::createInstance(m_world, parsedChar);
}

AnyCell* DatumReader::parseNumber(int radix)
{
	int peekChar = rdbuf()->sgetc();

	if (peekChar == EOF)
	{
		return EofObjectCell::instance();
	}
	else if (peekChar == '+')
	{
		return parsePositiveNumber(radix);
	}
	else if (peekChar == '-')
	{
		return parseNegativeNumber(radix);
	}
	else
	{
		return parseUnradixedNumber(radix);
	}
}

AnyCell* DatumReader::parsePositiveNumber(int radix)
{
	// Take the +
	rdbuf()->sbumpc();

	if (consumeLiteral(rdbuf(), "inf.0", true))
	{
		return FlonumCell::positiveInfinity(m_world);
	}
	else if (consumeLiteral(rdbuf(), "nan.0", true))
	{
		return FlonumCell::NaN(m_world);
	}

	try
	{
		return parseUnradixedNumber(radix, false);
	}
	catch(ReadErrorException)
	{
		// Clean up so we can backtrack as a symbol
		rdbuf()->sputbackc('+');
		throw;
	}
}

AnyCell* DatumReader::parseNegativeNumber(int radix)
{
	// Take the -
	rdbuf()->sbumpc();

	if (consumeLiteral(rdbuf(), "inf.0", true))
	{
		return FlonumCell::negativeInfinity(m_world);
	}
	else if (consumeLiteral(rdbuf(), "nan.0", true))
	{
		return FlonumCell::NaN(m_world);
	}

	try
	{
		return parseUnradixedNumber(radix, true);
	}
	catch(ReadErrorException)
	{
		// Clean up so we can backtrack as a symbol
		rdbuf()->sputbackc('-');
		throw;
	}
}

AnyCell* DatumReader::parseUnradixedNumber(int radix, bool negative)
{
	std::string numberString;

	takeWhile(rdbuf(), numberString, [=] (char c) -> bool {
		if ((c >= '0') && (c <= ('0' + std::max(10, radix) - 1)))
		{
			return true;
		}
		else if (radix > 10)
		{
			char lowerC = tolower(c);
			return (lowerC >= 'a') && (lowerC <= ('a' + radix - 10));
		}
		else
		{
			return false;
		}
	});

	if (numberString.empty() && (rdbuf()->sgetc() != '.'))
	{
		// Not valid
		throw MalformedDatumException(inputOffset(rdbuf()), "No valid number found after number prefix");
	}

	if (radix == 10)
	{
		int peekChar = rdbuf()->sgetc();

		if (peekChar == '.')
		{
			// Add the .
			numberString.push_back(rdbuf()->sbumpc());

			const auto previousSize = numberString.size();
			takeWhile(rdbuf(), numberString, [=] (char c) {
				return (c >= '0') && (c <= ('9'));
			});

			if (numberString.size() == 1)
			{
				// We just contain the "." - this isn't a valid number
				// This should re-parse as a symbol
				rdbuf()->sputbackc('.');
				throw MalformedDatumException(inputOffset(rdbuf()), "Decimal point with no trailing numbers");
			}

			if (previousSize != numberString.size())
			{
				// We took more numbers - we're not exact
				double doubleValue = std::strtod(numberString.c_str(), nullptr);

				if (negative)
				{
					doubleValue = -doubleValue;
				}

				return FlonumCell::fromValue(m_world, doubleValue);
			}
		}
		else if (peekChar == '/')
		{
			rdbuf()->sbumpc();
			std::string denomString;

			takeWhile(rdbuf(), denomString, [=] (char c) {
				return (c >= '0') && (c <= '9');
			});

			if (!denomString.empty())
			{
				const double numerValue = std::stod(numberString, nullptr);
				const double denomValue = std::stod(denomString, nullptr);

				double doubleValue = numerValue / denomValue;

				if (negative)
				{
					doubleValue = -doubleValue;
				}

				return FlonumCell::fromValue(m_world, doubleValue);
			}

			// Put the / back; it will treated as a symbol
			rdbuf()->sputbackc('/');
		}
	}

	std::int64_t intValue = std::strtoll(numberString.c_str(), nullptr, radix);

	if (negative)
	{
		intValue = -intValue;
	}

	return ExactIntegerCell::fromValue(m_world, intValue);
}

AnyCell* DatumReader::parseList(char closeChar)
{
	alloc::PairRef listHead(m_world, nullptr);
	alloc::PairRef listTail(m_world, nullptr);

	// Take the ( or [
	rdbuf()->sbumpc();

	while(true)
	{
		if (consumeWhitespace() == EOF)
		{
			throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while reading list");
		}

		int peekChar = rdbuf()->sgetc();

		if (peekChar == closeChar)
		{
			// Take the )
			rdbuf()->sbumpc();

			// Finished as a proper list
			if (listHead)
			{
				return listHead.data();
			}
			else
			{
				return EmptyListCell::instance();
			}
		}
		else if (peekChar == '.')
		{
			// Take the .
			rdbuf()->sbumpc();

			// Make sure they aren't a symbol
			if (isIdentifierChar(rdbuf()->sgetc()))
			{
				rdbuf()->sputbackc('.');
				// Fall through to parsing normal below
			}
			else
			{
				AnyCell *tailValue = parse();

				consumeWhitespace();

				if (rdbuf()->sbumpc() != closeChar)
				{
					throw MalformedDatumException(inputOffset(rdbuf()), "Improper list expected to terminate after tail datum");
				}

				if (!listHead)
				{
					return tailValue;
				}
				else
				{
					listTail->setCdr(tailValue);
					return listHead.data();
				}
			}
		}

		// Parse the next datum
		AnyCell *nextValue = parse();

		// Make a new tail pair
		auto tailPair = PairCell::createInstance(m_world, nextValue, EmptyListCell::instance());

		if (!listHead)
		{
			// We're the first pair!
			listHead.setData(tailPair);
			listTail.setData(tailPair);
		}
		else
		{
			// Move our tail forward
			listTail->setCdr(tailPair);
			listTail.setData(tailPair);
		}
	}
}

AnyCell* DatumReader::parseVector()
{
	alloc::StrongRefVector<AnyCell> elementRefs(m_world);

	// The ( is already taken

	while(true)
	{
		if (consumeWhitespace() == EOF)
		{
			throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while reading vector");
		}

		if (rdbuf()->sgetc() == ')')
		{
			// Take the )
			rdbuf()->sbumpc();

			// All done
			break;
		}

		elementRefs.push_back(parse());
	}

	const auto elementCount = elementRefs.size();
	auto *newElements = new AnyCell*[elementCount];
	std::memcpy(newElements, elementRefs.data(), sizeof(AnyCell*) * elementCount);

	return VectorCell::fromElements(m_world, newElements, elementCount);
}

AnyCell* DatumReader::parseBytevector()
{
	std::vector<std::uint8_t> elements;

	// The ( is already taken

	while(true)
	{
		if (consumeWhitespace() == EOF)
		{
			throw UnexpectedEofException(inputOffset(rdbuf()), "Unexpected end of input while reading bytevector");
		}

		if (rdbuf()->sgetc() == ')')
		{
			// Take the )
			rdbuf()->sbumpc();

			// All done
			break;
		}

		// Note that this is fairly inefficient as it needs to GC allocate the parsed datum and then immediately
		// discard it
		AnyCell *element = parse();

		if (auto exactInt = cell_cast<ExactIntegerCell>(element))
		{
			if ((exactInt->value() < 0) || (exactInt->value() > 255))
			{
				throw MalformedDatumException(inputOffset(rdbuf()), "Value out of byte range while reading bytevector");
			}

			elements.push_back(exactInt->value());
		}
		else
		{
			throw MalformedDatumException(inputOffset(rdbuf()), "Non-integer while reading bytevector");
		}
	}

	return BytevectorCell::fromData(m_world, elements.data(), elements.size());
}

AnyCell* DatumReader::parseDatumLabel(char firstDigit)
{
	std::string labelString({firstDigit});

	takeWhile(rdbuf(), labelString, [=] (char c) {
		return (c >= '0') && (c <= '9');
	});

	const long long labelNumber = std::strtoll(labelString.c_str(), nullptr, 10);

	int getChar = rdbuf()->sbumpc();

	if (getChar == '=')
	{
		// We're defining a new datum
		AnyCell *labelledDatum = parseDatum();

		// Use emplace/piecewise_construct as creating temporary StrongRefs is inefficient
		m_datumLabels.emplace(std::piecewise_construct, std::forward_as_tuple(labelNumber), std::forward_as_tuple(m_world, labelledDatum));

		return labelledDatum;
	}
	else if (getChar == '#')
	{
		// We're referencing a datum
		auto labelIt = m_datumLabels.find(labelNumber);

		if (labelIt == m_datumLabels.end())
		{
			throw MalformedDatumException(inputOffset(rdbuf()), "Undefined datum label");
		}

		return labelIt->second;
	}
	else
	{
		throw MalformedDatumException(inputOffset(rdbuf()), "Invalid datum label syntax");
	}
}

}
