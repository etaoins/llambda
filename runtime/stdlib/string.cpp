#include "binding/CharacterCell.h"
#include "binding/ListElementCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"
#include "core/fatal.h"

using namespace lliby;

extern "C"
{

StringCell *lliby_make_string(std::uint32_t length, UnicodeChar fill)
{
	return StringCell::fromFill(length, fill);
}

StringCell *lliby_string(const ListElementCell *argHead)
{
	ProperList<CharacterCell> charProperList(argHead);

	if (!charProperList.isValid())
	{
		_lliby_fatal("Non-character passed to (string)", argHead); 
	}

	std::list<UnicodeChar> unicodeCharList;

	for(auto charCell : charProperList)
	{
		unicodeCharList.push_back(charCell->unicodeChar());
	}

	return StringCell::fromUnicodeChars(unicodeCharList);
}

std::uint32_t lliby_string_length(const StringCell *string)
{
	return string->charLength();
}

std::int32_t lliby_string_ref(const StringCell *string, std::uint32_t index)
{
	UnicodeChar unicodeChar(string->charAt(index).codePoint());

	if (!unicodeChar.isValid())
	{
		_lliby_fatal("(string-ref) past end of string", string);
	}

	return unicodeChar.codePoint();
}

void lliby_string_set(StringCell *string, std::uint32_t index, UnicodeChar unicodeChar)
{
	if (!string->setCharAt(index, unicodeChar))
	{
		_lliby_fatal("(string-set!) past end of string", string);
	}
}

StringCell* lliby_string_append(ListElementCell *argHead)
{
	ProperList<StringCell> properList(argHead);

	if (!properList.isValid())
	{
		_lliby_fatal("Non-string passed to (string-append)", argHead);
	}

	// Use the std::list range constructor 
	std::list<const StringCell*> stringList(properList.begin(), properList.end()); 

	return StringCell::fromAppended(stringList);
}

}
