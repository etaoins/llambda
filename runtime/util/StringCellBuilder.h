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
	explicit StringCellBuilder(StringCell::CharLengthType charLength) :
		m_expectedChars(charLength)
	{
		// Four bytes is the largest encoding for a UTF-8 character
		m_outputBuffer = new std::uint8_t[charLength * 4];
		m_outputCursor = m_outputBuffer;
	}

	/**
	 * Explicitly deleted copy constructor
	 *
	 * Copying a StringCellBuilder would be an O(n) operation and there is no known use case for this.
	 */
	StringCellBuilder(const StringCellBuilder &other) = delete;

	StringCellBuilder(StringCellBuilder &&other)
	{
		m_outputBuffer = other.m_outputBuffer;
		m_expectedChars = other.m_expectedChars;
		m_outputCursor = other.m_outputCursor;

#ifndef NDEBUG
		m_actualChars = other.m_actualChars;
#endif

		// Prevent a double delete
		other.m_outputBuffer = nullptr;
	}

	~StringCellBuilder()
	{
		delete[] m_outputBuffer;
	}

	/**
	 * Returns the total byte length of UTF-8 encoded data appended to the builder
	 *
	 * @sa StringCell::maximumByteLength()
	 */
	std::size_t byteLength() const
	{
		return m_outputCursor - m_outputBuffer;
	}

	/**
	 * Returns the fully built StringCell instance allocated in the passed world
	 *
	 * It's an error if the number of appended characters does not match the character count passed to the builder
	 * constructor when this is called
	 */
	StringCell *result(World &world)
	{
		const std::size_t totalByteLength = m_outputCursor - m_outputBuffer;

		assert(m_actualChars == m_expectedChars);
		assert(totalByteLength <= StringCell::maximumByteLength());

		// Create a new string to write in to
		auto newString = StringCell::createUninitialized(world, totalByteLength, m_expectedChars);

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
		assert(character.isValid());
		m_actualChars++;
#endif
		m_outputCursor = utf8::appendChar(character, m_outputCursor);
		return *this;
	}

private:
	std::uint8_t *m_outputBuffer;
	StringCell::CharLengthType m_expectedChars;
	std::uint8_t *m_outputCursor;

#ifndef NDEBUG
	StringCell::CharLengthType m_actualChars = 0;
#endif
};

}

#endif
