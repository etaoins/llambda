#include "dynamic/init.h"
#include "dynamic/ParameterProcedureCell.h"

namespace lliby
{
namespace dynamic
{

void init()
{
	// Register our record classes
	ParameterProcedureCell::registerRecordClass();
}

}
}
