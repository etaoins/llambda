#include <string>
#include <sstream>

#include "core/init.h"
#include "core/World.h"

#include "writer/DisplayDatumWriter.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/CharCell.h"

#include "alloc/cellref.h"
#include "assertions.h"
#include "stubdefinitions.h"

namespace
{
using namespace lliby;

std::string displayFormFor(const AnyCell *datum)
{
	std::ostringstream outputStream;

	DisplayDatumWriter writer(outputStream);
	writer.render(datum);

	return outputStream.str();
}

void assertForm(const AnyCell *datum, std::string expected)
{
	ASSERT_EQUAL(displayFormFor(datum), expected);
}

SymbolCell *symbolFor(World &world, const char *utf8String)
{
	return SymbolCell::fromUtf8StdString(world, utf8String);
}

StringCell *stringFor(World &world, const char *utf8String)
{
	return StringCell::fromUtf8StdString(world, utf8String);
}

void testSymbol(World &world)
{
	assertForm(symbolFor(world, u8"Hello"), u8"Hello");
	assertForm(symbolFor(world, u8"HelloWorldThisRequiresHeapAllocation"), u8"HelloWorldThisRequiresHeapAllocation");
	assertForm(symbolFor(world, u8"λ"), u8"λ");
	assertForm(symbolFor(world, u8"Hello, world"), u8"Hello, world");
	assertForm(symbolFor(world, u8"Back\\slash"), u8"Back\\slash");
	assertForm(symbolFor(world, u8"P|pe"), u8"P|pe");
	assertForm(symbolFor(world, u8"Quo\"te"), u8"Quo\"te");
	assertForm(symbolFor(world, u8""), u8"");
}

void testString(World &world)
{
	assertForm(stringFor(world, u8"Hello"), u8"Hello");
	assertForm(stringFor(world, u8"λ"), u8"λ");
	assertForm(stringFor(world, u8"Hello, world"), u8"Hello, world");
	assertForm(stringFor(world, u8"Hello\nworld"), u8"Hello\nworld");
	assertForm(stringFor(world, u8"Hello\bworld"), u8"Hello\bworld");
	assertForm(stringFor(world, u8"Hello\"world"), u8"Hello\"world");
	assertForm(stringFor(world, u8""), u8"");
}

void testCharacter(World &world)
{
	assertForm(CharCell::createInstance(world, UnicodeChar(0x0a)), "\n");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x0d)), "\r");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x20)), " ");
	assertForm(CharCell::createInstance(world, UnicodeChar('A')), "A");
	assertForm(CharCell::createInstance(world, UnicodeChar('a')), "a");
	assertForm(CharCell::createInstance(world, UnicodeChar('1')), "1");
	assertForm(CharCell::createInstance(world, UnicodeChar(')')), ")");
	assertForm(CharCell::createInstance(world, UnicodeChar(0x03bb)), u8"\u03bb");
}

void testAll(World &world)
{
	testSymbol(world);
	testString(world);
	testCharacter(world);
}

}

int main(int argc, char *argv[])
{
	lliby_init();
	
	lliby::World::launchWorld(&testAll);
}
