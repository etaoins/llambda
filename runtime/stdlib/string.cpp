#include "binding/BoxedCharacter.h"
#include "binding/BoxedListElement.h"
#include "binding/BoxedString.h"
#include "binding/ProperList.h"
#include "core/fatal.h"

using namespace lliby;

extern "C"
{

BoxedString *lliby_make_string(std::uint32_t length, UnicodeChar fill)
{
	return BoxedString::fromFill(length, fill);
}

BoxedString *lliby_string(const BoxedListElement *argHead)
{
	ProperList<BoxedCharacter> charProperList(argHead);

	if (!charProperList.isValid())
	{
		_lliby_fatal("Non-character passed to (string)", argHead); 
	}

	std::list<UnicodeChar> unicodeCharList;

	for(auto boxedChar : charProperList)
	{
		unicodeCharList.push_back(boxedChar->unicodeChar());
	}

	return BoxedString::fromUnicodeChars(unicodeCharList);
}

std::uint32_t lliby_string_length(const BoxedString *string)
{
	return string->charLength();
}

std::int32_t lliby_string_ref(const BoxedString *string, std::uint32_t index)
{
	UnicodeChar unicodeChar(string->charAt(index).codePoint());

	if (!unicodeChar.isValid())
	{
		_lliby_fatal("(string-ref) past end of string", string);
	}

	return unicodeChar.codePoint();
}

void lliby_string_set(BoxedString *string, std::uint32_t index, UnicodeChar unicodeChar)
{
	if (!string->setCharAt(index, unicodeChar))
	{
		_lliby_fatal("(string-set!) past end of string", string);
	}
}

BoxedString* lliby_string_append(BoxedListElement *argHead)
{
	ProperList<BoxedString> properList(argHead);

	if (!properList.isValid())
	{
		_lliby_fatal("Non-string passed to (string-append)", argHead);
	}

	// Use the std::list range constructor 
	std::list<const BoxedString*> stringList(properList.begin(), properList.end()); 

	return BoxedString::fromAppended(stringList);
}

}
