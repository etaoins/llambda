#include "binding/CharacterCell.h"
#include "binding/ListElementCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

StringCell *lliby_make_string(World &world, std::uint32_t length, UnicodeChar fill)
{
	return StringCell::fromFill(world, length, fill);
}

// Note we can't use RestArgument here because invalid lists can be passed in via our (list->string) alias
StringCell *lliby_string(World &world, ListElementCell *argHead)
{
	ProperList<CharacterCell> charProperList(argHead);

	if (!charProperList.isValid())
	{
		signalError(world, "Non-character passed to (string)", {argHead}); 
	}

	std::vector<UnicodeChar> unicodeCharList;
	unicodeCharList.reserve(charProperList.length());

	for(auto charCell : charProperList)
	{
		unicodeCharList.push_back(charCell->unicodeChar());
	}

	return StringCell::fromUnicodeChars(world, unicodeCharList);
}

std::uint32_t lliby_string_length(const StringCell *string)
{
	return string->charLength();
}

std::int32_t lliby_string_ref(World &world, StringCell *string, std::uint32_t index)
{
	UnicodeChar unicodeChar(string->charAt(index).codePoint());

	if (!unicodeChar.isValid())
	{
		signalError(world, "(string-ref) past end of string", {string});
	}

	return unicodeChar.codePoint();
}

void lliby_string_set(World &world, StringCell *string, std::uint32_t index, UnicodeChar unicodeChar)
{
	if (string->isGlobalConstant())
	{
		signalError(world, "(string-set!) on a string literal", {string});
	}

	if (!string->setCharAt(index, unicodeChar))
	{
		signalError(world, "(string-set!) past end of string", {string});
	}
}

StringCell* lliby_string_append(World &world, RestArgument<StringCell> *argHead)
{
	ProperList<StringCell> properList(argHead);

	// Use the std::vector range constructor 
	std::vector<StringCell*> stringList(properList.begin(), properList.end()); 

	return StringCell::fromAppended(world, stringList);
}

}
