#ifndef _LLIBY_UTIL_STRINGCELLBUILDER_H
#define _LLIBY_UTIL_STRINGCELLBUILDER_H

#include "binding/StringCell.h"
#include "unicode/utf8.h"
#include "unicode/UnicodeChar.h"

#include <cstring>
#include <cassert>

namespace lliby
{

/**
 * Builds a string cell of a known number of characters by appending single Unicode characters
 */
class StringCellBuilder
{
public:
	/**
	 * Constructs a StringCellBuilder for a string of a known number of characters
	 *
	 * It's an error if the total number of characters appended to the builder does not match charLength when result()
	 * is called
	 */
	explicit StringCellBuilder(std::uint32_t charLength) :
		m_expectedChars(charLength)
	{
		// Four bytes is the largest encoding for a UTF-8 chacracter
		m_outputBuffer = new std::uint8_t[charLength * 4];
		m_outputCursor = m_outputBuffer;
	}

	/**
	 * Explicitly deleted copy constructor
	 *
	 * Copying a StringCellBuilder would be an O(n) operation and there is no known use case for this.
	 */
	StringCellBuilder(const StringCellBuilder &other) = delete;

	StringCellBuilder(StringCellBuilder &&other) = default;

	~StringCellBuilder()
	{
		delete[] m_outputBuffer;
	}

	/**
	 * Returns the fully built StringCell instance allocated in the passed world
	 *
	 * It's an error if the number of appended characters does not match the character count passed to the builder
	 * constructor when this is called
	 */
	StringCell *result(World &world)
	{
#ifndef NDEBUG
		assert(m_actualChars == m_expectedChars);
#endif
		const std::uint32_t totalByteLength = m_outputCursor - m_outputBuffer;

		// Create a new string to write in to
		auto newString = StringCell::createUninitialized(world, totalByteLength);
		newString->setCharLength(m_expectedChars);

		std::uint8_t *utf8Data = newString->utf8Data();

		memcpy(utf8Data, m_outputBuffer, totalByteLength);

		return newString;
	}

	/**
	 * Appends Unicode character to the builder
	 */
	StringCellBuilder& operator<<(UnicodeChar character)
	{
#ifndef NDEBUG
		m_actualChars++;
#endif
		m_outputCursor = utf8::appendUtf8Char(character, m_outputCursor);
		return *this;
	}

private:
	std::uint8_t *m_outputBuffer;
	std::uint32_t m_expectedChars;
	std::uint8_t *m_outputCursor;

#ifndef NDEBUG
	std::uint32_t m_actualChars = 0;
#endif
};

}

#endif
