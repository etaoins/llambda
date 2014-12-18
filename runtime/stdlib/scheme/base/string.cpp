#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"

#include "core/error.h"

#include "util/assertSliceValid.h"
#include "util/stringCompare.h"
#include "util/StringCellBuilder.h"

using namespace lliby;

extern "C"
{

StringCell *llbase_make_string(World &world, std::uint32_t length, UnicodeChar fill)
{
	if (!fill.isValid())
	{
		signalError(world, ErrorCategory::Default, "(make-string) with invalid fill character");
	}

	return StringCell::fromFill(world, length, fill);
}

StringCell *llbase_string(World &world, RestValues<CharCell> *charProperList)
{
	StringCellBuilder builder(charProperList->size());

	for(auto charCell : *charProperList)
	{
		UnicodeChar character(charCell->unicodeChar());

		if (!character.isValid())
		{
			signalError(world, ErrorCategory::Default, "(string) with invalid character", {charProperList});
		}

		builder << charCell->unicodeChar();
	}

	return builder.result(world);
}

std::uint32_t llbase_string_length(const StringCell *string)
{
	return string->charLength();
}

std::int32_t llbase_string_ref(World &world, StringCell *string, std::uint32_t index)
{
	UnicodeChar unicodeChar(string->charAt(index).codePoint());

	if (!unicodeChar.isValid())
	{
		signalError(world, ErrorCategory::Range, "(string-ref) past end of string", {string});
	}

	return unicodeChar.codePoint();
}

void llbase_string_set(World &world, StringCell *string, std::uint32_t index, UnicodeChar unicodeChar)
{
	if (string->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(string-set!) on a string literal", {string});
	}

	if (!unicodeChar.isValid())
	{
		signalError(world, ErrorCategory::Default, "(string-set!) with invalid character");
	}

	if (!string->setCharAt(index, unicodeChar))
	{
		signalError(world, ErrorCategory::Range, "(string-set!) past end of string", {string});
	}
}

StringCell* llbase_string_append(World &world, RestValues<StringCell> *argHead)
{
	std::vector<StringCell*> stringList(argHead->begin(), argHead->end());

	return StringCell::fromAppended(world, stringList);
}

ProperList<CharCell>* llbase_string_to_list(World &world, StringCell *sourceString, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(string->list)", sourceString, sourceString->charLength(), start, end);

	// Get the unboxed Unicode chars
	std::vector<UnicodeChar> unboxedChars(sourceString->unicodeChars(start, end));

	return ProperList<CharCell>::emplaceValues(world, unboxedChars);
}

// This is also used to implement (string-copy)
StringCell* llbase_substring(World &world, StringCell *sourceString, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(substring)", sourceString, sourceString->charLength(), start, end);
	return sourceString->copy(world, start, end);
}

void llbase_string_mutating_copy(World &world, StringCell *to, std::int64_t at, StringCell *from, std::int64_t start, std::int64_t end)
{
	if (to->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(string-copy!) on string literal", {to});
	}

	assertSliceValid(world, "(string-copy!)", from, from->charLength(), start, end);
	assertSliceValid(world, "(string-copy!)", to, to->charLength(), at, at + (end - start));

	to->replace(at, from, start, end);
}

void llbase_string_mutating_fill(World &world, StringCell *string, UnicodeChar fill, std::int64_t start, std::int64_t end)
{
	if (string->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(string-fill!) on string literal", {string});
	}

	if (!fill.isValid())
	{
		signalError(world, ErrorCategory::Default, "(string-fill!) with invalid character");
	}

	assertSliceValid(world, "(string-fill!)", string, string->charLength(), start, end);

	string->fill(fill, start, end);
}

bool llbase_string_equal(StringCell *value1, StringCell *value2, RestValues<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return *value1 == *value2; });
}

bool llbase_string_lt(StringCell *value1, StringCell *value2, RestValues<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2) < 0; });
}

bool llbase_string_gt(StringCell *value1, StringCell *value2, RestValues<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2) > 0; });
}

bool llbase_string_lte(StringCell *value1, StringCell *value2, RestValues<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2) <= 0; });
}

bool llbase_string_gte(StringCell *value1, StringCell *value2, RestValues<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2) >= 0; });
}

}
