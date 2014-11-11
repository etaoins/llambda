#ifndef _LLIBY_BINDING_STRINGCELL_H
#define _LLIBY_BINDING_STRINGCELL_H

#include "AnyCell.h"
#include <list>
#include <vector>
#include <ostream>
#include <string>

#include "unicode/UnicodeChar.h"

namespace lliby
{

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
	static StringCell* fromUtf8StdString(World &world, const std::string &str);

	static StringCell* fromUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength);
	static StringCell* fromValidatedUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength, std::uint32_t charLength);

	/**
	 * Creates a StringCell using a SharedByteArray
	 *
	 * If possible the new StringCell will be constructed sharing the passed SharedByteArray. If that occurs then the
	 * byteArray will have its reference count incremented.
	 */
	static StringCell* withUtf8ByteArray(World &world, SharedByteArray *byteArray, std::uint32_t byteLength);

	static StringCell* fromFill(World &world, std::uint32_t length, UnicodeChar fill);
	static StringCell* fromSymbol(World &world, SymbolCell *symbol);
	
	static StringCell* fromAppended(World &world, std::vector<StringCell*> &strings);
	static StringCell* fromAppended(World &world, const std::vector<StringCell*> &strings)
	{
		std::vector<StringCell*> stringsCopy(strings);
		return fromAppended(world, stringsCopy);
	}

	StringCell* copy(World &world, std::int64_t start = 0, std::int64_t end = -1); 

	UnicodeChar charAt(std::uint32_t offset) const;
	bool setCharAt(std::uint32_t offset, UnicodeChar unicodeChar);

	bool fill(UnicodeChar unicodeChar, std::int64_t start = 0, std::int64_t end = -1);
	bool replace(std::uint32_t offset, const StringCell *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	std::vector<UnicodeChar> unicodeChars(std::int64_t start = 0, std::int64_t end = -1) const;
	
	bool operator==(const StringCell &other) const;
	
	bool operator!=(const StringCell &other) const
	{
		return !(*this == other);
	}

	// Returns and integer less than, equal to or greater than zero if the string
	// less than, equal to or greater than the other string
	int compare(const StringCell *other, CaseSensitivity cs = CaseSensitivity::Sensitive) const;

	BytevectorCell *toUtf8Bytevector(World &world, std::int64_t start = 0, std::int64_t end = -1);

	StringCell *toUppercaseString(World &world);
	StringCell *toLowercaseString(World &world);
	StringCell *toCaseFoldedString(World &world);
	
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
		std::uint32_t charCount;

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
	CharRange charRange(std::int64_t start, std::int64_t end = -1);

protected:
	StringCell(std::uint32_t byteLength, std::uint32_t charLength, std::uint16_t allocSlackBytes) :
		AnyCell(CellTypeId::String),
		m_allocSlackBytes(allocSlackBytes),
		m_charLength(charLength),
		m_byteLength(byteLength)
	{
	}
	
	std::uint8_t* utf8Data();

	static const std::uint32_t InlineDataSize = 12;

	// Creates an uninitialized cell with the given size
	static StringCell* createUninitialized(World &world, std::uint32_t byteLength, std::uint32_t charLength);

	const std::uint8_t *charPointer(std::uint32_t cahrOffset, const std::uint8_t *startFrom, std::uint32_t startOffset);
	const std::uint8_t *charPointer(std::uint32_t charOffset);

	bool replaceBytes(const CharRange &range, const std::uint8_t *pattern, unsigned int patternBytes, unsigned int count);

	int compareCaseSensitive(const StringCell *other) const;
	int compareCaseInsensitive(const StringCell *other) const;

	StringCell *toConvertedString(World &world, UnicodeChar (UnicodeChar::* converter)() const); 
	
	static size_t inlineDataSize();
	bool dataIsInline() const;
	
	void setByteLength(std::uint32_t newByteLength)
	{
		m_byteLength = newByteLength;
	}

	void setAllocSlackBytes(std::uint16_t newAllocSlackBytes)
	{
		m_allocSlackBytes = newAllocSlackBytes;
	}
};

class HeapStringCell : public StringCell
{
	friend class StringCell;
	friend class SymbolCell;
#include "generated/HeapStringCellMembers.h"
private:
	HeapStringCell(SharedByteArray *byteArray, std::uint32_t byteLength, std::uint32_t charLength, std::uint16_t allocSlackBytes) :
		StringCell(byteLength, charLength, allocSlackBytes),
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
	InlineStringCell(std::uint32_t byteLength, std::uint32_t charLength) :
		StringCell(byteLength, charLength, InlineDataSize - byteLength)
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

