#include <istream>
#include <string>
#include <ctype.h>

namespace
{
	/**
	 * Consumes all whitespace in a stream until non-whitespace or EOF is encountered
	 */
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

	/**
	 * Takes characters from a string while they satisfy a predicate
	 *
	 * @param  inStream   Stream to read from
	 * @param  accum      String to accumulate the result in. Any existing contents are preserved and the new data is
	 *                    appended at the end
	 * @param  predicate  Function taking a character and returning a boolean predicate indicating if the character
	 *                    should be taken.
	 */
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

	/**
	 * Attempts to consume a given literal string from the stream
	 *
	 * @param  inStream         Stream to read from
	 * @param  expected         Expected literal to consume
	 * @param  caseInsensitive  Indicates if the expected string should be matched case insensitively. This only works
	 *                          if "expected" is pure ASCII
	 * @return True if the literal was consumed, false otherwise. If the literal wasn't consumed then any read
	 *         characters are put back on the stream
	 */
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
