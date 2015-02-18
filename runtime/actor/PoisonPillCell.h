#ifndef _LLIBY_ACTOR_POISONPILLCELL_H
#define _LLIBY_ACTOR_POISONPILLCELL_H

#include "binding/RecordCell.h"

namespace lliby
{
namespace actor
{

/**
 * Singleton cell instance representing a poison pill messae
 */
class PoisonPillCell : public RecordCell
{
public:
	PoisonPillCell();

	static PoisonPillCell *instance();
};

}
}

#endif
