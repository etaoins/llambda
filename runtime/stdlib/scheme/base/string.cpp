#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"

#include "core/error.h"

#include "util/rangeAssertions.h"
#include "util/stringCompare.h"
#include "util/StringCellBuilder.h"

using namespace lliby;

extern "C"
{

StringCell *llbase_make_string(World &world, std::int64_t length, UnicodeChar fill)
{
	assertLengthValid(world, "(make-string)", "string length", StringCell::maximumCharLength(), length);

	return StringCell::fromFill(world, length, fill);
}

StringCell *llbase_string(World &world, RestValues<CharCell> *charProperList)
{
	StringCellBuilder builder(charProperList->size());

	for(auto charCell : *charProperList)
	{
		builder << charCell->unicodeChar();
	}

	return builder.result(world);
}

std::uint32_t llbase_string_length(const StringCell *string)
{
	return string->charLength();
}

std::int32_t llbase_string_ref(World &world, StringCell *string, std::int64_t index)
{
	assertIndexValid(world, "(string-ref)", string, string->charLength(), index);

	return string->charAt(index).codePoint();
}

StringCell* llbase_string_append(World &world, RestValues<StringCell> *argHead)
{
	std::vector<StringCell*> stringList(argHead->begin(), argHead->end());

	return StringCell::fromAppended(world, stringList);
}

ProperList<CharCell>* llbase_string_to_list(World &world, StringCell *sourceString, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(string->list)", sourceString, sourceString->charLength(), start, end);

	return ProperList<CharCell>::emplaceValues(world, sourceString->charRange(start, end));
}

// This is also used to implement (string-copy)
StringCell* llbase_substring(World &world, StringCell *sourceString, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(substring)", sourceString, sourceString->charLength(), start, end);
	return sourceString->copy(world, start, end);
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
