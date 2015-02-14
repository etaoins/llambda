#include "actor/PoisonPillCell.h"

namespace lliby
{
namespace actor
{

namespace
{
	PoisonPillCell Instance;
}

PoisonPillCell::PoisonPillCell() :
	RecordCell(EmptyRecordLikeClassId, true, nullptr, GarbageState::GlobalConstant)
{
}

PoisonPillCell* PoisonPillCell::instance()
{
	return &Instance;
}

}
}
