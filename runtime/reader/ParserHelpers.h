#include <istream>
#include <string>
#include <ctype.h>

#include "unicode/utf8.h"
#include "unicode/utf8/InvalidByteSequenceException.h"

namespace
{
	using namespace lliby;

	/**
	 * Returns the byte offset of the given rdbuf
	 *
	 * @param  rdbuf  Stream buffer to fetch the offset of
	 */
	int inputOffset(std::streambuf *rdbuf)
	{
		return rdbuf->pubseekoff(0, std::ios::cur, std::ios::in);
	}

	/**
	 * Discards characters from a string while they satisfy a predicate
	 *
	 * @param  rdbuf      Stream buffer to read from
	 * @param  predicate  Function taking a character and returning a boolean predicate indicating if the character
	 *                    should be discared.
	 */
	template<class F>
	void discardWhile(std::streambuf *rdbuf, F predicate)
	{
		while(true)
		{
			int nextChar = rdbuf->sgetc();

			if ((nextChar == EOF) || !predicate(nextChar))
			{
				return;
			}

			rdbuf->sbumpc();
		}
	}

	/**
	 * Takes characters from a string while they satisfy a predicate
	 *
	 * @param  rdbuf      Stream buffer to read from
	 * @param  accum      String to accumulate the result in. Any existing contents are preserved and the new data is
	 *                    appended at the end
	 * @param  predicate  Function taking a character and returning a boolean predicate indicating if the character
	 *                    should be taken.
	 */
	template<class F>
	void takeWhile(std::streambuf *rdbuf, std::string &accum, F predicate)
	{
		while(true)
		{
			int nextChar = rdbuf->sgetc();

			if ((nextChar == EOF) || !predicate(nextChar))
			{
				return;
			}

			rdbuf->sbumpc();
			accum.push_back(nextChar);
		}
	}

	/**
	 * Takes a case insensitive hexadecimal string from the stream
	 */
	std::string takeHexadecimal(std::streambuf *rdbuf)
	{
		std::string result;

		takeWhile(rdbuf, result, [] (char c)
		{
			char lowerC = tolower(c);
			return ((lowerC >= '0') && (lowerC <= '9')) || ((lowerC >= 'a') && (lowerC <= 'f'));
		});

		return result;
	}

	/**
	 * Takes a decimal number from the stream
	 */
	void takeDecimal(std::streambuf *rdbuf, std::string &accum)
	{
		takeWhile(rdbuf, accum, [=] (char c) {
			return (c >= '0') && (c <= '9');
		});
	}

	std::string takeDecimal(std::streambuf *rdbuf)
	{
		std::string result;
		takeDecimal(rdbuf, result);
		return result;
	}

	/**
	 * Attempts to consume a given literal string from the stream
	 *
	 * @param  rdbuf            Stream buffer to read from
	 * @param  expected         Expected literal to consume
	 * @param  caseInsensitive  Indicates if the expected string should be matched case insensitively. This only works
	 *                          if "expected" is pure ASCII
	 * @return True if the literal was consumed, false otherwise. If the literal wasn't consumed then any read
	 *         characters are put back on the stream
	 */
	bool consumeLiteral(std::streambuf *rdbuf, const char *expected, bool caseInsensitive = false)
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

			const int actualChar = rdbuf->sgetc();

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
					rdbuf->sputbackc(accum[i - 1]);
				}

				return false;
			}

			rdbuf->sbumpc();
			accum.push_back(actualChar);
		}
	}

	/**
	 * Skips the next UTF-8 byte sequence from the input
	 *
	 * @param  rdbuf  Stream buffer to skip
	 */
	void skipUtf8Character(std::streambuf *rdbuf)
	{
		const int headerByte = rdbuf->sgetc();
		int sequenceBytes = utf8::bytesInSequence(headerByte);

		if (sequenceBytes > 0)
		{
			while(sequenceBytes--)
			{
				rdbuf->sbumpc();
			}
		}
		else
		{
			const int offset = inputOffset(rdbuf);

			rdbuf->sbumpc();
			throw utf8::InvalidHeaderByteException(0, offset);
		}
	}
}
