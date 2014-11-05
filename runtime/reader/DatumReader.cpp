#include "DatumReader.h"
#include "ReadErrorException.h"

#include <cassert>
#include <cstdlib>
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

#include "alloc/cellref.h"

#include "unicode/utf8.h"

#include "ReadErrorException.h"

namespace lliby
{

namespace
{
	void consumeWhitespace(std::istream &inStream)
	{
		while(true)
		{
			int peekChar = inStream.peek();

			if ((peekChar == '\r') || (peekChar == '\t') || (peekChar == ' '))
			{
				inStream.get();
			}
			else
			{
				// All done
				return;
			}
		}
	}

	template<class F>
	void takeWhile(std::istream &inStream, std::string &accum, F predicate)
	{
		while(true)
		{
			int nextChar = inStream.get();

			if (nextChar == EOF)
			{
				// Out of data
				return;
			}

			if (predicate(nextChar))
			{
				accum.push_back(nextChar);
			}
			else
			{
				inStream.putback(nextChar);
				return;
			}
		}
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
				throw ReadErrorException("End of input without closing quote for string-like");
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
					throw ReadErrorException("End of input during backslash escaped sequence");
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
						std::string hexCode;

						takeWhile(inStream, hexCode, [] (char c)
						{
							char lowerC = tolower(c);

							return ((lowerC >= '0') && (lowerC <= '9')) || ((lowerC >= 'a') && (lowerC <= 'f'));
						});

						nextChar = inStream.get();

						if (nextChar != ';')
						{
							throw ReadErrorException("Hex escape not terminated with ;");
						}
						else if (hexCode.empty())
						{
							throw ReadErrorException("Empty hex escape");
						}

						UnicodeChar escapedChar(strtol(hexCode.c_str(), nullptr, 16));
						utf8::appendChar(escapedChar, std::back_inserter(accum));
					}
					break;

				case '\n':
					// Consume the whitespace at the beginning of the next line
					consumeWhitespace(inStream);
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

	bool consumeLiteral(std::istream &inStream, const char *expected, bool caseInsensitive = false)
	{
		std::string accum;

		while(true)
		{
			const int expectedChar = expected[accum.size()];

			if (expectedChar == 0)
			{
				// All done
				return true;
			}

			const int actualChar = inStream.get();
			accum.push_back(actualChar);

			if (actualChar == EOF)
			{
				// Ran out of stream
				return false;
			}

			bool matched;

			if (caseInsensitive)
			{
				matched = tolower(expectedChar) == tolower(actualChar);
			}
			else
			{
				matched = expectedChar == actualChar;
			}

			if (!matched)
			{
				// Put everything back
				for(auto i = accum.size(); i > 0; i--)
				{
					inStream.putback(accum[i - 1]);
				}

				return false;
			}
		}
	}
}

AnyCell* DatumReader::parse(int defaultRadix)
{
	consumeWhitespace(m_inStream);

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
	else
	{
		// Everything else is a symbol
		return parseSymbol();
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
	else if (getChar == '!')
	{
		if (consumeLiteral(m_inStream, "unit"))
		{
			return UnitCell::instance();
		}
	}

	throw ReadErrorException("Unrecognized # datum");
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

	takeWhile(m_inStream, symbolData, [] (char c) {
		return
			// Has to be above the control character and whitespace range
			(c > 0x20) &&
			// Can't be a literal backslash
			(c != 0x5c) &&
			// Can't be "
			(c != '"') &&
			// Can't be |
			(c != '|') &&
			// Can't be DEL or above
			(c < 0x7f);
	});

	if (symbolData.empty())
	{
		// Not implemented!
		throw ReadErrorException("Unrecognized start character");
	}

	return SymbolCell::fromUtf8StdString(m_world, symbolData);
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
		throw ReadErrorException("No valid number found after number prefix");
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
		consumeWhitespace(m_inStream);

		if (m_inStream.eof())
		{
			throw ReadErrorException("Unexpected end of input while reading list");
		}

		if (m_inStream.peek() == closeChar)
		{
			// All done
			if (listHead)
			{
				return listHead.data();
			}
			else
			{
				return EmptyListCell::instance();
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

}
