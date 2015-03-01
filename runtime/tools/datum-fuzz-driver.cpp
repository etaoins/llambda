#include <iostream>
#include <sstream>
#include <cstdlib>

#include "core/World.h"
#include "core/init.h"
#include "../tests/stubdefinitions.h"

#include "binding/EofObjectCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/SymbolCell.h"
#include "binding/StringCell.h"
#include "binding/PairCell.h"
#include "binding/CharCell.h"
#include "binding/VectorCell.h"
#include "binding/BytevectorCell.h"

#include "alloc/cellref.h"

#include "reader/DatumReader.h"
#include "reader/ReadErrorException.h"

#include "writer/ExternalFormDatumWriter.h"
#include "unicode/utf8/InvalidByteSequenceException.h"

namespace
{
	using namespace lliby;

	/**
	 * Compares two data for approximate equality
	 *
	 * This is nearly the same as AnyCell::isEqual except:
	 *
	 * - Flonums only need to be within 0.001 of each other. Some fraction literals cannot be converted to decimal form
	 *   and back without losing precision.
	 * - Data without external form never compare as equal. These should not appear when reading.
	 */
	bool isEqualish(const AnyCell *left, const AnyCell *right)
	{
		if (left == right)
		{
			return true;
		}

		// These require more than address comparison by eqv?
		if (auto leftInteger = cell_cast<ExactIntegerCell>(left))
		{
			if (auto rightInteger = cell_cast<ExactIntegerCell>(right))
			{
				return leftInteger->value() == rightInteger->value();
			}
		}
		else if (auto leftFlonum = cell_cast<FlonumCell>(left))
		{
			if (auto rightFlonum = cell_cast<FlonumCell>(right))
			{
				const double leftValue = leftFlonum->value();
				const double rightValue = rightFlonum->value();

				if (std::isnan(leftValue) && std::isnan(rightValue))
				{
					// Both are NaN
					return true;
				}

				// Distinguish positive and negative zero
				if (std::signbit(leftValue) != std::signbit(rightValue))
				{
					return false;
				}

				// Do a fuzzy comparison. Also check for equality to handle infinities
				return (leftFlonum->value() == rightFlonum->value()) ||
					(std::fabs(leftFlonum->value() - rightFlonum->value()) < 0.001);
			}
		}
		else if (auto leftSymbol = cell_cast<SymbolCell>(left))
		{
			if (auto rightSymbol = cell_cast<SymbolCell>(right))
			{
				return *leftSymbol == *rightSymbol;
			}
		}
		else if (auto leftString = cell_cast<StringCell>(left))
		{
			if (auto rightString = cell_cast<StringCell>(right))
			{
				return *leftString == *rightString;
			}
		}
		else if (auto leftChar = cell_cast<CharCell>(left))
		{
			if (auto rightChar = cell_cast<CharCell>(right))
			{
				return leftChar->unicodeChar() == rightChar->unicodeChar();
			}
		}
		else if (auto leftPair = cell_cast<PairCell>(left))
		{
			if (auto rightPair = cell_cast<PairCell>(right))
			{
				return isEqualish(leftPair->car(), rightPair->car()) &&
						isEqualish(leftPair->cdr(), rightPair->cdr());
			}
		}
		else if (auto leftVector = cell_cast<VectorCell>(left))
		{
			if (auto rightVector = cell_cast<VectorCell>(right))
			{
				if (leftVector->length() != rightVector->length())
				{
					return false;
				}

				for(VectorCell::LengthType i = 0; i < leftVector->length(); i++)
				{
					if (!isEqualish(leftVector->elements()[i], rightVector->elements()[i]))
					{
						return false;
					}
				}

				return true;
			}
		}
		else if (auto leftBytevector = cell_cast<BytevectorCell>(left))
		{
			if (auto rightBytevector = cell_cast<BytevectorCell>(right))
			{
				return leftBytevector->isEqual(rightBytevector);
			}
		}

		return false;
	}

	void testStdin(World &world)
	{
		DatumReader stdinReader(world, std::cin);

		while(true)
		{
			alloc::AnyRef firstReadRef(world);

			try
			{
				// Read from stdin
				AnyCell *parsedDatum = stdinReader.parse();
				firstReadRef.setData(parsedDatum);
			}
			catch(const ReadErrorException &e)
			{
				std::cerr << e.message() << std::endl;

				// The input stream should be past the bad datum
				continue;
			}
			catch(const utf8::InvalidByteSequenceException &e)
			{
				std::cerr << e.message() << std::endl;

				// The input stream should be past the bad byte sequence
				continue;
			}

			if (firstReadRef.data() == EofObjectCell::instance())
			{
				// Ran out of input
				return;
			}

			// Render to a string stream
			std::ostringstream outStream;
			ExternalFormDatumWriter writer(outStream);

			writer.render(firstReadRef);

			// Re-parse the datum
			std::istringstream inStream(outStream.str());
			DatumReader secondReader(world, inStream);

			alloc::AnyRef secondReadRef(world, secondReader.parse());

			// Make sure they're equal
			if (!isEqualish(firstReadRef, secondReadRef))
			{
				ExternalFormDatumWriter errorWriter(std::cerr);

				std::cerr << "Datum was not reparsed as same value" << std::endl << std::endl;

				std::cerr << "First read:" << std::endl;
				errorWriter.render(firstReadRef);
				std::cerr << std::endl << std::endl;

				std::cerr << "Second read:" << std::endl;
				errorWriter.render(secondReadRef);
				std::cerr << std::endl;

				std::abort();
			}
		}

	}
}

int main(int argc, char *argv[])
{
	if (argc > 1)
	{
		std::cout << "Usage: " << argv[0] << std::endl;
		std::cout << std::endl;
		std::cout << "This reads a sequence of Scheme data from stdin and attempts to parse it. If a" << std::endl;
		std::cout << "read error or UTF-8 error is encountered then parsing will resume at the next" << std::endl;
		std::cout << "character. If another exception is raised, e.g. by the C++ standard library, " << std::endl;
		std::cout << "then execution will abort" << std::endl;
		std::cout << std::endl;
		std::cout << "For every successful parse the datum will be re-rendered to an internal buffer" << std::endl;
		std::cout << "and then reparsed. The reparsed datum is then tested for equality to ensure that" << std::endl;
		std::cout << "it was rendered correctly. If any exception is raised while reparsing then" << std::endl;
		std::cout << "execution will abort" << std::endl;
		std::cout << std::endl;
		std::cout << "This is intended for use by automated fuzzing tools such as afl-fuzz to detect" << std::endl;
		std::cout << "corner cases in datum parsing and rendering." << std::endl;

		return 1;
	}

	llcore_run(testStdin, argc, argv);
}
