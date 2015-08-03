#include <iostream>
#include <stdlib.h>
#include <limits>

#include "core/init.h"
#include "core/World.h"

#include "binding/AnyCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProcedureCell.h"
#include "binding/CharCell.h"
#include "binding/BytevectorCell.h"
#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ProperList.h"
#include "binding/VectorCell.h"
#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "binding/RecordCell.h"
#include "binding/EofObjectCell.h"
#include "binding/PortCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/HashMapCell.h"

#include "port/StringOutputPort.h"
#include "actor/Mailbox.h"

#include "alloc/StrongRefVector.h"
#include "alloc/cellref.h"
#include "hash/DatumHash.h"
#include "hash/DatumHashTree.h"
#include "writer/ExternalFormDatumWriter.h"

#include "stubdefinitions.h"

using namespace lliby;

namespace
{

std::uint8_t* utf8Bytes(const char *str)
{
	return (std::uint8_t*)(str);
}

bool hasUnstableHash(AnyCell *cell)
{
	return MailboxCell::isInstance(cell) || PortCell::isInstance(cell);
}

HashMapCell* hashMapCellFromValues(World &world, std::initializer_list<std::pair<AnyCell *&, AnyCell *&>> values)
{
	HashMapCell *hashMapCell = HashMapCell::createEmptyInstance(world);

	for(const auto &pair : values)
	{
		DatumHashTree *newTree = DatumHashTree::assoc(hashMapCell->datumHashTree() , pair.first, pair.second);

		DatumHashTree::unref(hashMapCell->datumHashTree());
		hashMapCell->setDatumHashTree(newTree);
	}

	return hashMapCell;
}

void testAll(World &world)
{
	alloc::StrongRefVector<AnyCell> testValues(world);

	auto recordClass = ProcedureCell::registerRuntimeRecordClass(0, {});

	testValues.push_back(StringCell::fromUtf8StdString(world, ""));
	testValues.push_back(StringCell::fromUtf8StdString(world, "Hello"));
	testValues.push_back(StringCell::fromUtf8StdString(world, "HELLO"));
	testValues.push_back(StringCell::fromUtf8Data(world, utf8Bytes("Hell\0o"), 6));
	testValues.push_back(StringCell::fromUtf8StdString(world, "Hell"));
	testValues.push_back(StringCell::fromUtf8StdString(world, u8"☃🐉"));
	testValues.push_back(StringCell::fromUtf8StdString(world, u8"сфmmциist gязэtiйgs!"));
	testValues.push_back(StringCell::fromUtf8StdString(world, u8"СФMMЦИIST GЯЗЭTIЙGS!"));

	testValues.push_back(SymbolCell::fromUtf8StdString(world, ""));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, "Hello"));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, "HELLO"));
	testValues.push_back(SymbolCell::fromUtf8Data(world, utf8Bytes("Hell\0o"), 6));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, "Hell"));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, u8"☃🐉"));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, u8"сфmmциist gязэtiйgs!"));
	testValues.push_back(SymbolCell::fromUtf8StdString(world, u8"СФMMЦИIST GЯЗЭTIЙGS!"));

	testValues.push_back(BooleanCell::falseInstance());
	testValues.push_back(BooleanCell::trueInstance());

	testValues.push_back(ExactIntegerCell::fromValue(world, std::numeric_limits<std::int64_t>::min()));
	testValues.push_back(ExactIntegerCell::fromValue(world, -1));
	testValues.push_back(ExactIntegerCell::fromValue(world, 0));
	testValues.push_back(ExactIntegerCell::fromValue(world, 1));
	testValues.push_back(ExactIntegerCell::fromValue(world, std::numeric_limits<std::int64_t>::max()));

	testValues.push_back(FlonumCell::fromValue(world, std::numeric_limits<double>::quiet_NaN()));
	testValues.push_back(FlonumCell::fromValue(world, std::numeric_limits<double>::min()));
	testValues.push_back(FlonumCell::fromValue(world, -1.0));
	testValues.push_back(FlonumCell::fromValue(world, -0.0));
	testValues.push_back(FlonumCell::fromValue(world, +0.0));
	testValues.push_back(FlonumCell::fromValue(world, 1.0));
	testValues.push_back(FlonumCell::fromValue(world, std::numeric_limits<double>::max()));

	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x0)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x20)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x41)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x5a)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x61)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x7a)));
	testValues.push_back(CharCell::createInstance(world, UnicodeChar(0x2603)));

	testValues.push_back(BytevectorCell::fromData(world, utf8Bytes(""), 0));
	testValues.push_back(BytevectorCell::fromData(world, utf8Bytes("Hello"), 5));
	testValues.push_back(BytevectorCell::fromData(world, utf8Bytes("HELLO"), 5));
	testValues.push_back(BytevectorCell::fromData(world, utf8Bytes("Hell\0o"), 6));

	testValues.push_back(EmptyListCell::instance());

	testValues.push_back(PairCell::createInstance(world, testValues[1], testValues[1]));
	testValues.push_back(PairCell::createInstance(world, testValues[0], testValues[0]));
	testValues.push_back(PairCell::createInstance(world, testValues[0], testValues[1]));
	testValues.push_back(PairCell::createInstance(world, testValues[1], testValues[0]));

	testValues.push_back(ProperList<ExactIntegerCell>::emplaceValues(world, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
	testValues.push_back(ProperList<ExactIntegerCell>::emplaceValues(world, {10, 9, 8, 7, 6, 5, 4, 3, 2, 1}));

	testValues.push_back(BytevectorCell::fromData(world, nullptr, 0));

	std::uint8_t ascendingArray[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	testValues.push_back(BytevectorCell::fromData(world, ascendingArray, sizeof(ascendingArray)));

	std::uint8_t descendingArray[] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
	testValues.push_back(BytevectorCell::fromData(world, descendingArray, sizeof(descendingArray)));

	testValues.push_back(VectorCell::fromElements(world, nullptr, 0));

	AnyCell **elements = new AnyCell*[6] {testValues[0], testValues[1], testValues[2], testValues[3], testValues[4]};
	testValues.push_back(VectorCell::fromElements(world, elements, 5));

	elements = new AnyCell*[6] {testValues[4], testValues[3], testValues[2], testValues[1], testValues[0]};
	testValues.push_back(VectorCell::fromElements(world, elements, 5));

	testValues.push_back(ProcedureCell::createInstance(
				world,
				ProcedureCell::EmptyRecordLikeClassId,
				true,
				nullptr,
				reinterpret_cast<void*>(1)));

	testValues.push_back(ProcedureCell::createInstance(
				world,
				ProcedureCell::EmptyRecordLikeClassId,
				true,
				nullptr,
				reinterpret_cast<void*>(1)));

	testValues.push_back(ProcedureCell::createInstance(
				world,
				ProcedureCell::EmptyRecordLikeClassId,
				true,
				nullptr,
				reinterpret_cast<void*>(2)));

	testValues.push_back(ProcedureCell::createInstance(
				world,
				recordClass,
				true,
				nullptr,
				reinterpret_cast<void*>(1)));

	testValues.push_back(ProcedureCell::createInstance(
				world,
				recordClass,
				true,
				nullptr,
				reinterpret_cast<void*>(2)));


	testValues.push_back(MailboxCell::createInstance(world, std::make_shared<actor::Mailbox>()));
	testValues.push_back(MailboxCell::createInstance(world, std::make_shared<actor::Mailbox>()));
	testValues.push_back(MailboxCell::createInstance(world, std::make_shared<actor::Mailbox>()));

	testValues.push_back(UnitCell::instance());

	auto recordClass1 = RecordLikeCell::registerRuntimeRecordClass(0, {});
	testValues.push_back(RecordCell::createInstance(world, recordClass1, true, nullptr));
	testValues.push_back(RecordCell::createInstance(world, recordClass1, true, nullptr));

	auto recordClass2 = RecordLikeCell::registerRuntimeRecordClass(2, {});
	testValues.push_back(RecordCell::createInstance(world, recordClass2, true, nullptr));

	auto recordClass3 = RecordLikeCell::registerRuntimeRecordClass(4, {});
	testValues.push_back(RecordCell::createInstance(world, recordClass3, true, nullptr));
	testValues.push_back(RecordCell::createInstance(world, recordClass3, true, nullptr));

	auto recordClass4 = RecordLikeCell::registerRuntimeRecordClass(8, {});
	testValues.push_back(RecordCell::createInstance(world, recordClass4, true, nullptr));

	testValues.push_back(EofObjectCell::instance());

	testValues.push_back(PortCell::createInstance(world, new StringOutputPort));
	testValues.push_back(PortCell::createInstance(world, new StringOutputPort));
	testValues.push_back(PortCell::createInstance(world, new StringOutputPort));

	alloc::StringRef errorString(world, StringCell::fromUtf8StdString(world, u8"Test error"));
	testValues.push_back(ErrorObjectCell::createInstance(
				world,
				errorString,
				EmptyListCell::asProperList<AnyCell>()));

	testValues.push_back(ErrorObjectCell::createInstance(
				world,
				errorString,
				EmptyListCell::asProperList<AnyCell>()));

	testValues.push_back(ErrorObjectCell::createInstance(
				world,
				errorString,
				EmptyListCell::asProperList<AnyCell>(),
				ErrorCategory::Arity));

	testValues.push_back(hashMapCellFromValues(world, {}));

	testValues.push_back(hashMapCellFromValues(world, {
				{testValues[0], testValues[1]},
				{testValues[2], testValues[3]},
				{testValues[4], testValues[5]},
				{testValues[6], testValues[7]},
	}));

	testValues.push_back(hashMapCellFromValues(world, {
				{testValues[6], testValues[7]},
				{testValues[4], testValues[5]},
				{testValues[2], testValues[3]},
				{testValues[0], testValues[1]},
	}));

	testValues.push_back(hashMapCellFromValues(world, {
				{testValues[4], testValues[5]},
				{testValues[6], testValues[7]},
				{testValues[0], testValues[1]},
				{testValues[2], testValues[3]},
	}));


	testValues.push_back(hashMapCellFromValues(world, {
				{testValues[8], testValues[9]},
				{testValues[10], testValues[11]},
				{testValues[12], testValues[13]},
				{testValues[14], testValues[15]},
	}));

	for(std::size_t i = 0; i < testValues.size(); i++)
	{
		for(std::size_t j = i; j < testValues.size(); j++)
		{
			DatumHash hashFunction;

			auto valueI = testValues[i];
			auto valueJ = testValues[j];

			auto hashI = hashFunction(valueI);
			auto hashJ = hashFunction(valueJ);

			if (valueI->isEqual(valueJ))
			{
				if (hashI != hashJ)
				{
					ExternalFormDatumWriter writer(std::cerr);

					std::cerr << "Values ";
					writer.render(valueI);
					std::cerr << " and ";
					writer.render(valueJ);
					std::cerr << " have inequal hash codes ";
					std::cerr << hashI << " and " << hashJ << " but compare as equal" << std::endl;

					exit(-1);
				}
			}
			else if (hashI == hashJ)
			{
				ExternalFormDatumWriter writer(std::cerr);

				std::cerr << "Values '";
				writer.render(valueI);
				std::cerr << "' and '";
				writer.render(valueJ);
				std::cerr << "' have equal hash codes ";
				std::cerr << hashI << " and " << hashJ << " but compare as inequal" << std::endl;

				if (hasUnstableHash(valueI) || hasUnstableHash(valueJ))
				{
					std::cerr << "One of the values has an unstable hash code so this may be spurious" << std::endl;
				}
				else
				{
					exit(-1);
				}
			}
		}
	}
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
