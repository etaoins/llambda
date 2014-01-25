#ifndef _LLIBY_CORE_DYNAMIC_H
#define _LLIBY_CORE_DYNAMIC_H

namespace lliby
{

class ProcedureCell;
class DatumCell;

}

extern "C"
{

using namespace lliby;

void _lliby_dynamicenv_push();
void _lliby_dynamicenv_set_value(ProcedureCell *, DatumCell *);
void _lliby_dynamicenv_pop();


}

#endif
