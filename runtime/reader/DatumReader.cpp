#include "DatumReader.h"
#include "ReadErrorException.h"
#include "ParserHelpers.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <ctype.h>
#include <iterator>
#include <iostream>

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

	std::string takeQuotedStringLike(std::istream &inStream, char quoteChar)
	{
		std::string accum;

		while(true)
		{
			int nextChar = inStream.get();

			if (nextChar == EOF)
			{
				// Out of data without closing quote
				throw ReadErrorException(inStream.tellg(), "End of input without closing quote for string-like");
			}

			if (nextChar == quoteChar)
			{
				return accum;
			}
			else if (nextChar == '\\')
			{
				// This is a quoted character
				nextChar = inStream.get();

				if (nextChar == EOF)
				{
					// Out of data without closing quote
					throw ReadErrorException(inStream.tellg(), "End of input during backslash escaped sequence");
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
						std::string hexCode = takeHexidecimal(inStream);

						nextChar = inStream.get();

						if (nextChar != ';')
						{
							throw ReadErrorException(inStream.tellg(), "Hex escape not terminated with ;");
						}
						else if (hexCode.empty())
						{
							throw ReadErrorException(inStream.tellg(), "Empty hex escape");
						}

						UnicodeChar escapedChar(strtol(hexCode.c_str(), nullptr, 16));
						utf8::appendChar(escapedChar, std::back_inserter(accum));
					}
					break;

				case '\n':
					// Discard the intraline whitespace at the beginning of the next line
					discardWhile(inStream, [] (char c)
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
	consumeWhitespace();

	int peekChar = m_inStream.peek();

	if (peekChar == EOF)
	{
		return EofObjectCell::instance();
	}
	else if ((peekChar >= '0') && (peekChar <= '9'))
	{
		return parseUnradixedNumber(defaultRadix);
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
		m_inStream.get();

		if (m_inStream.peek() == '@')
		{
			return parseSymbolShorthand("unquote-splicing");
		}
		else
		{
			m_inStream.putback(',');
			return parseSymbolShorthand("unquote");
		}
	}
	else
	{
		// Everything else is a symbol
		return parseSymbol();
	}
}

void DatumReader::consumeWhitespace()
{
	while(true)
	{
		int peekChar = m_inStream.peek();

		if ((peekChar == '\r') || (peekChar == '\n') || (peekChar == '\t') || (peekChar == ' '))
		{
			m_inStream.get();
		}
		else if (peekChar == ';')
		{
			// Consume until the end of the line
			int getChar;
			do
			{
				getChar = m_inStream.get();
			}
			while((getChar != '\n') && (getChar != EOF));
		}
		else if (peekChar == '#')
		{
			// This could be one of the R7RS comment types
			m_inStream.get();

			peekChar = m_inStream.peek();
			if (peekChar == ';')
			{
				// Discard the commented out datum
				m_inStream.get();
				consumeWhitespace();
				parse();
			}
			else if (peekChar == '|')
			{
				m_inStream.get();
				consumeBlockComment();
				consumeWhitespace();
			}
			else
			{
				m_inStream.putback('#');
			}

			return;
		}
		else
		{
			// All done
			return;
		}
	}
}

void DatumReader::consumeBlockComment()
{
	int commentDepth = 1;

	while(true)
	{
		int firstChar = m_inStream.get();

		if (firstChar == EOF)
		{
			return;
		}
		else if (firstChar == '#')
		{
			if (m_inStream.get() == '|')
			{
				++commentDepth;
			}
		}
		else if (firstChar == '|')
		{
			if (m_inStream.get() == '#')
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
	m_inStream.get();

	int getChar = m_inStream.get();

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
		consumeLiteral(m_inStream, "rue");
		return BooleanCell::trueInstance();
	}
	else if (getChar == 'f')
	{
		consumeLiteral(m_inStream, "alse");
		return BooleanCell::falseInstance();
	}
	else if (getChar == '(')
	{
		return parseVector();
	}
	else if (getChar == '!')
	{
		if (consumeLiteral(m_inStream, "unit"))
		{
			return UnitCell::instance();
		}
	}
	else if (getChar == 'u')
	{
		// This is the rest of #u8(, not just a sad face
		if (consumeLiteral(m_inStream, "8("))
		{
			return parseBytevector();
		}
	}
	else if (getChar == '\\')
	{
		return parseChar();
	}
	else if (getChar == EOF)
	{
		throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while parsing # datum");
	}

	throw ReadErrorException(m_inStream.tellg(), "Unrecognized # datum");
}

AnyCell* DatumReader::parseEnclosedSymbol()
{
	// Consume the |
	m_inStream.get();

	return SymbolCell::fromUtf8StdString(m_world, takeQuotedStringLike(m_inStream, '|'));
}

AnyCell* DatumReader::parseString()
{
	// Consume the "
	m_inStream.get();

	return StringCell::fromUtf8StdString(m_world, takeQuotedStringLike(m_inStream, '"'));
}

AnyCell* DatumReader::parseSymbol()
{
	std::string symbolData;

	takeWhile(m_inStream, symbolData, isIdentifierChar);

	if (symbolData.empty())
	{
		throw ReadErrorException(m_inStream.tellg(), "Unrecognized start character");
	}
	else if (symbolData == ".")
	{
		throw ReadErrorException(m_inStream.tellg(), ". reserved for terminating improper lists");
	}

	return SymbolCell::fromUtf8StdString(m_world, symbolData);
}

AnyCell* DatumReader::parseSymbolShorthand(const std::string &expanded)
{
	// Consume the shorthand
	m_inStream.get();

	alloc::SymbolRef expandedSymbol(m_world, SymbolCell::fromUtf8StdString(m_world, expanded));
	AnyCell *innerDatum = parse();

	return ProperList<AnyCell>::create(m_world, {expandedSymbol, innerDatum});
}

AnyCell* DatumReader::parseChar()
{
	int nextChar = m_inStream.get();

	if (nextChar == EOF)
	{
		throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while reading character");
	}
	else if ((nextChar == 'a') && consumeLiteral(m_inStream, "larm"))
	{
		return CharCell::createInstance(m_world, 0x07);
	}
	else if ((nextChar == 'b') && consumeLiteral(m_inStream, "ackspace"))
	{
		return CharCell::createInstance(m_world, 0x08);
	}
	else if ((nextChar == 'd') && consumeLiteral(m_inStream, "elete"))
	{
		return CharCell::createInstance(m_world, 0x7f);
	}
	else if ((nextChar == 'e') && consumeLiteral(m_inStream, "scape"))
	{
		return CharCell::createInstance(m_world, 0x1b);
	}
	else if ((nextChar == 'n') && consumeLiteral(m_inStream, "ewline"))
	{
		return CharCell::createInstance(m_world, 0x0a);
	}
	else if ((nextChar == 'n') && consumeLiteral(m_inStream, "ull"))
	{
		return CharCell::createInstance(m_world, 0x00);
	}
	else if ((nextChar == 'r') && consumeLiteral(m_inStream, "eturn"))
	{
		return CharCell::createInstance(m_world, 0x0d);
	}
	else if ((nextChar == 's') && consumeLiteral(m_inStream, "pace"))
	{
		return CharCell::createInstance(m_world, 0x20);
	}
	else if ((nextChar == 't') && consumeLiteral(m_inStream, "ab"))
	{
		return CharCell::createInstance(m_world, 0x09);
	}
	else if ((nextChar == 'x') || (nextChar == 'X'))
	{
		std::string hexCode = takeHexidecimal(m_inStream);

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

	m_inStream.read(reinterpret_cast<char*>(&byteBuffer[1]), seqBytes - 1);

	if (m_inStream.gcount() != (seqBytes - 1))
	{
		throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while reading character");
	}

	utf8::validateData(byteBuffer, &byteBuffer[seqBytes]);

	const std::uint8_t *scanPtr = byteBuffer;
	UnicodeChar parsedChar = utf8::decodeChar(&scanPtr);

	if ((parsedChar.codePoint() < '0') || ((parsedChar.codePoint() > '9')))
	{
		// If this is a non-digit then it can't be followed by an identifier character
		if (isIdentifierChar(m_inStream.peek()))
		{
			throw ReadErrorException(m_inStream.tellg(), "Unrecognized character name");
		}
	}

	return CharCell::createInstance(m_world, parsedChar);
}

AnyCell* DatumReader::parseNumber(int radix)
{
	int peekChar = m_inStream.peek();

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
	if (consumeLiteral(m_inStream, "+inf.0", true))
	{
		return FlonumCell::positiveInfinity(m_world);
	}
	else if (consumeLiteral(m_inStream, "+nan.0", true))
	{
		return FlonumCell::NaN(m_world);
	}

	// Take the +
	m_inStream.get();

	try
	{
		return parseUnradixedNumber(radix, false);
	}
	catch(ReadErrorException)
	{
		// Clean up so we can backtrack as a symbol
		m_inStream.clear();
		m_inStream.putback('+');
		throw;
	}
}

AnyCell* DatumReader::parseNegativeNumber(int radix)
{
	if (consumeLiteral(m_inStream, "-inf.0", true))
	{
		return FlonumCell::negativeInfinity(m_world);
	}
	else if (consumeLiteral(m_inStream, "-nan.0", true))
	{
		return FlonumCell::NaN(m_world);
	}

	// Take the -
	m_inStream.get();

	try
	{
		return parseUnradixedNumber(radix, true);
	}
	catch(ReadErrorException)
	{
		// Clean up so we can backtrack as a symbol
		m_inStream.clear();
		m_inStream.putback('-');
		throw;
	}
}

AnyCell* DatumReader::parseUnradixedNumber(int radix, bool negative)
{
	std::string numberString;

	takeWhile(m_inStream, numberString, [=] (char c) -> bool {
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

	if (numberString.empty())
	{
		// Not valid
		throw ReadErrorException(m_inStream.tellg(), "No valid number found after number prefix");
	}

	if (radix == 10)
	{
		int peekChar = m_inStream.peek();

		if (peekChar == '.')
		{
			// Add the .
			numberString.push_back(m_inStream.get());

			const auto previousSize = numberString.size();
			takeWhile(m_inStream, numberString, [=] (char c) {
				return (c >= '0') && (c <= ('9'));
			});

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
			m_inStream.get();
			std::string denomString;

			takeWhile(m_inStream, denomString, [=] (char c) {
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
			m_inStream.putback('/');
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
	m_inStream.get();

	while(true)
	{
		consumeWhitespace();

		if (m_inStream.eof())
		{
			throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while reading list");
		}

		int peekChar = m_inStream.peek();

		if (peekChar == closeChar)
		{
			// Take the )
			m_inStream.get();

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
			m_inStream.get();

			// Make sure they aren't a symbol
			if (isIdentifierChar(m_inStream.peek()))
			{
				m_inStream.putback('.');
				// Fall through to parsing normal below
			}
			else
			{
				AnyCell *tailValue = parse();

				consumeWhitespace();

				if (m_inStream.get() != closeChar)
				{
					throw ReadErrorException(m_inStream.tellg(), "Improper list expected to terminate after tail datum");
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
		consumeWhitespace();

		if (m_inStream.eof())
		{
			throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while reading vector");
		}

		if (m_inStream.peek() == ')')
		{
			// Take the )
			m_inStream.get();

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
		consumeWhitespace();

		if (m_inStream.eof())
		{
			throw ReadErrorException(m_inStream.tellg(), "Unexpected end of input while reading bytevector");
		}

		if (m_inStream.peek() == ')')
		{
			// Take the )
			m_inStream.get();

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
				throw ReadErrorException(m_inStream.tellg(), "Value out of byte range while reading bytevector");
			}

			elements.push_back(exactInt->value());
		}
		else
		{
			throw ReadErrorException(m_inStream.tellg(), "Non-integer while reading bytevector");
		}
	}

	return BytevectorCell::fromData(m_world, elements.data(), elements.size());
}

}
