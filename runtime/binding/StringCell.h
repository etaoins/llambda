#ifndef _LLIBY_BINDING_STRINGCELL_H
#define _LLIBY_BINDING_STRINGCELL_H

#include "AnyCell.h"
#include "SharedByteArray.h"

#include <list>
#include <vector>
#include <ostream>
#include <string>
#include <limits>

#include "unicode/UnicodeChar.h"

namespace lliby
{
namespace alloc
{
class Heap;
}

class World;
class ImplicitSharingTest;
class StringCellBuilder;

class StringCell : public AnyCell
{
	friend class SymbolCell;
	friend class ImplicitSharingTest;
	friend class StringCellBuilder;
#include "generated/StringCellMembers.h"
public:
	using CharLengthType = decltype(m_charLength);
	using ByteLengthType = decltype(m_byteLength);
	using SliceIndexType = std::int64_t;

	constexpr static CharLengthType maximumCharLength()
	{
		return std::numeric_limits<CharLengthType>::max();
	}

	constexpr static ByteLengthType maximumByteLength()
	{
		return std::numeric_limits<ByteLengthType>::max();
	}

	static StringCell* fromUtf8StdString(World &world, const std::string &str);

	static StringCell* fromUtf8Data(World &world, const std::uint8_t *data, ByteLengthType byteLength);
	static StringCell* fromValidatedUtf8Data(World &world, const std::uint8_t *data, ByteLengthType byteLength, CharLengthType charLength);

	/**
	 * Creates a StringCell using a SharedByteArray
	 *
	 * If possible the new StringCell will be constructed sharing the passed SharedByteArray. If that occurs then the
	 * byteArray will have its reference count incremented.
	 */
	static StringCell* withUtf8ByteArray(World &world, SharedByteArray *byteArray, ByteLengthType byteLength);

	static StringCell* fromFill(World &world, CharLengthType length, UnicodeChar fill);
	static StringCell* fromSymbol(World &world, SymbolCell *symbol);

	static StringCell* fromAppended(World &world, std::vector<StringCell*> &strings);
	static StringCell* fromAppended(World &world, const std::vector<StringCell*> &strings)
	{
		std::vector<StringCell*> stringsCopy(strings);
		return fromAppended(world, stringsCopy);
	}

	StringCell* copy(World &world, SliceIndexType start = 0, SliceIndexType end = -1);
	StringCell* copy(alloc::Heap &heap);

	UnicodeChar charAt(CharLengthType offset) const;
	bool setCharAt(CharLengthType offset, UnicodeChar unicodeChar);

	bool fill(UnicodeChar unicodeChar, SliceIndexType start = 0, SliceIndexType end = -1);
	bool replace(CharLengthType offset, const StringCell *from, SliceIndexType fromStart = 0, SliceIndexType fromEnd = -1);

	std::vector<UnicodeChar> unicodeChars(SliceIndexType start = 0, SliceIndexType end = -1) const;

	bool operator==(const StringCell &other) const;
	
	bool operator!=(const StringCell &other) const
	{
		return !(*this == other);
	}

	/**
	 * Returns and integer less than, equal to or greater than zero if the string less than, equal to or greater than
     * the other string
	 */
	int compare(const StringCell *other) const;
	int compare(const StringCell *other, UnicodeChar (*converter)(UnicodeChar)) const;

	/**
	 * Constructs a new string converted char-by-char using the passed converter function
	 */
	StringCell *toConvertedString(World &world, UnicodeChar (*converter)(UnicodeChar));

	BytevectorCell *toUtf8Bytevector(World &world, SliceIndexType start = 0, SliceIndexType end = -1);

	const std::uint8_t* constUtf8Data() const;

	std::string toUtf8StdString() const
	{
		return std::string(reinterpret_cast<const char*>(constUtf8Data()), byteLength());
	}

	void finalizeString();

	struct CharRange
	{
		const std::uint8_t *startPointer;
		const std::uint8_t *endPointer;
		CharLengthType charCount;

		bool isNull() const
		{
			return startPointer == nullptr;
		};

		unsigned int byteCount() const
		{
			return endPointer - startPointer;
		}

		void relocate(ptrdiff_t byteDelta)
		{
			startPointer += byteDelta;
			endPointer += byteDelta;
		}
	};

	/**
	 * Returns information about a range of characters
	 */
	CharRange charRange(SliceIndexType start, SliceIndexType end = -1);

protected:
	StringCell(ByteLengthType byteLength, CharLengthType charLength) :
		AnyCell(CellTypeId::String),
		m_charLength(charLength),
		m_byteLength(byteLength)
	{
	}
	
	std::uint8_t* utf8Data();

	// Creates an uninitialized cell with the given size
	static StringCell* createUninitialized(World &world, ByteLengthType byteLength, CharLengthType charLength);

	const std::uint8_t *charPointer(CharLengthType charOffset, const std::uint8_t *startFrom, ByteLengthType startOffset);
	const std::uint8_t *charPointer(CharLengthType charOffset);

	bool replaceBytes(const CharRange &range, const std::uint8_t *pattern, unsigned int patternBytes, unsigned int count);
	
	static size_t inlineDataSize();
	bool dataIsInline() const;
	
	void setByteLength(ByteLengthType newByteLength)
	{
		m_byteLength = newByteLength;
	}

	std::size_t byteCapacity() const;
};

class HeapStringCell : public StringCell
{
	friend class StringCell;
	friend class SymbolCell;
#include "generated/HeapStringCellMembers.h"
private:
	HeapStringCell(SharedByteArray *byteArray, ByteLengthType byteLength, CharLengthType charLength) :
		StringCell(byteLength, charLength),
		m_heapByteArray(byteArray)
	{
	}
	
	void setHeapByteArray(SharedByteArray* newHeapByteArray)
	{
		m_heapByteArray = newHeapByteArray;
	}
};

class InlineStringCell : public StringCell
{
	friend class StringCell;
	friend class SymbolCell;
#include "generated/InlineStringCellMembers.h"
private:
	InlineStringCell(ByteLengthType byteLength, CharLengthType charLength) :
		StringCell(byteLength, charLength)
	{
	}
};

inline std::ostream & operator<<(std::ostream &os, const StringCell* stringCell)
{
	// We are not NULL terminated by default
	os.write(reinterpret_cast<const char*>(stringCell->constUtf8Data()), stringCell->byteLength());
	return os;
}

}

#endif

