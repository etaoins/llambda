#ifndef _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H
#define _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H

#include "binding/ProcedureCell.h"

namespace lliby
{

class World;

namespace dynamic
{

class EscapeProcedureCell : public ProcedureCell
{
public:
	/**
	 * Creates a new escape procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param world               Current world pointer
	 *                            re-entering Scheme,
	 */
	static EscapeProcedureCell *createInstance(World &world);
	
	/**
	 * Registers the record class for the escape procedure's closure
	 *
	 * This is called by dynamic::init() at startup; this should not be directly invoked
	 */
	static void registerRecordClass();
};

}

}

#endif
