#include "dynamic/init.h"
#include "dynamic/ParameterProcedureCell.h"

namespace lliby
{
namespace dynamic
{

void init()
{
	// Register our closure record classes
	ParameterProcedureCell::registerRecordClass();
}

}
}
