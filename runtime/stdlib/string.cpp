#include "binding/CharCell.h"
#include "binding/ListElementCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

#include "core/error.h"

#include "util/assertSliceValid.h"

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
	ProperList<CharCell> charProperList(argHead);

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

ListElementCell* lliby_string_to_list(World &world, StringCell *sourceString, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(string->list)", sourceString, sourceString->charLength(), start, end);

	// Get the unboxed Unicode chars
	std::vector<UnicodeChar> unboxedChars(sourceString->unicodeChars(start, end));

	// Allocate space for the boxed chars
	alloc::RangeAlloc allocation = alloc::allocateRange(world, unboxedChars.size());
	auto allocIt = allocation.begin();

	// Box all of the characters
	std::vector<AnyCell*> boxedChars;
	boxedChars.reserve(unboxedChars.size());

	for(auto unboxedChar : unboxedChars)
	{
		boxedChars.push_back(new (*allocIt++) CharCell(unboxedChar));
	}

	return ListElementCell::createProperList(world, boxedChars);
}

// This is also used to implement (string-copy)
StringCell* lliby_substring(World &world, StringCell *sourceString, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(substring)", sourceString, sourceString->charLength(), start, end);
	return sourceString->copy(world, start, end);
}

StringCell* lliby_string_upcase(World &world, StringCell *sourceString)
{
	return sourceString->toUppercaseString(world);
}

StringCell* lliby_string_downcase(World &world, StringCell *sourceString)
{
	return sourceString->toLowercaseString(world);
}

StringCell* lliby_string_foldcase(World &world, StringCell *sourceString)
{
	return sourceString->toCaseFoldedString(world);
}

}
