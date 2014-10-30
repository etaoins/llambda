#include <iostream>
#include <cassert>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/UnitCell.h"
#include "binding/CharCell.h"

#include "unicode/utf8.h"
#include "unicode/utf8/InvalidByteSequenceException.h"
#include "util/utf8ExceptionToSchemeError.h"

#include "port/AbstractPort.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	std::istream* portCellToInputStream(World &world, PortCell *portCell)
	{
		AbstractPort *port = portCell->port();

		if (!port->isInputPort())
		{
			signalError(world, "Attempted to read from non-input port", {portCell});
		}

		if (!port->isInputPortOpen())
		{
			signalError(world, "Attempted to read from closed input port", {portCell});
		}

		return port->inputStream();
	}

	AnyCell *eofObject()
	{
		return UnitCell::instance();
	}

	AnyCell *readUtf8Character(World &world, const char *procName, std::istream *inputStream, bool putBack = false)
	{
		int headerChar = inputStream->get();

		if (headerChar == EOF)
		{
			return eofObject();
		}

		int seqBytes = utf8::bytesInSequence(headerChar);

		if (seqBytes < 1)
		{
			if (putBack)
			{
				// Put the bad header byte back
				inputStream->putback(headerChar);
			}

			utf8ExceptionToSchemeError(world, procName, utf8::InvalidByteSequenceException(0, 0, "Invalid header byte"));
			signalError(world, "Invalid UTF-8 header byte in (read-char)");
		}

		char charBuffer[utf8::LongestByteSequence];
		const std::uint8_t* utf8DataBufffer = reinterpret_cast<std::uint8_t*>(charBuffer);

		charBuffer[0] = headerChar;

		// Read the rest of the sequence in
		bool readSuccess = inputStream->read(&charBuffer[1], seqBytes - 1).good();

		if (putBack)
		{
			// Put all of our read data back even if the read failed or the UTF-8 byte sequence is invalid
			inputStream->clear();

			for(int i = inputStream->gcount(); i >= 0; i--)
			{
				inputStream->putback(charBuffer[i]);
			}
		}

		if (!readSuccess)
		{
			// End of stream mid-character
			return eofObject();
		}

		try
		{
			// Ensure the character is valid
			utf8::validateData(utf8DataBufffer, utf8DataBufffer + seqBytes);
		}
		catch (utf8::InvalidByteSequenceException &e)
		{
			if (!putBack)
			{
				// Put the bytes following the error back.  This allows the input stream to be recovered starting at the
				// next byte sequence if Scheme catches the UTF-8 error
				for(int i = seqBytes - 1; i > e.endOffset(); i--)
				{
					inputStream->putback(charBuffer[i]);
				}
			}

			utf8ExceptionToSchemeError(world, procName, e);
		}

		return CharCell::createInstance(world, utf8::decodeChar(&utf8DataBufffer));
	}
}

extern "C"
{

AnyCell *lliby_read_u8(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);

	int readChar = portStream->get();

	if (readChar == EOF)
	{
		return eofObject();
	}
	else
	{
		return ExactIntegerCell::fromValue(world, readChar);
	}
}

AnyCell *lliby_peek_u8(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);

	int peekChar = portStream->peek();

	if (peekChar == EOF)
	{
		return eofObject();
	}
	else
	{
		return ExactIntegerCell::fromValue(world, peekChar);
	}
}

AnyCell *lliby_read_char(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);
	return readUtf8Character(world, "(read-char)", portStream, false);
}

AnyCell *lliby_peek_char(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);
	return readUtf8Character(world, "(peek-char)", portStream, true);
}

}
