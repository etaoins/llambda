#ifndef _LLIBY_ALLOC_CELLREFTYPES_H
#define _LLIBY_ALLOC_CELLREFTYPES_H

/************************************************************
 * This file is generated by typegen. Do not edit manually. *
 ************************************************************/

#include "binding/AnyCell.h"
#include "alloc/StrongRef.h"

namespace lliby
{
namespace alloc
{

typedef StrongRef<AnyCell> AnyRef;
typedef StrongRef<ListElementCell> ListElementRef;
typedef StrongRef<PairCell> PairRef;
typedef StrongRef<StringCell> StringRef;
typedef StrongRef<InlineStringCell> InlineStringRef;
typedef StrongRef<HeapStringCell> HeapStringRef;
typedef StrongRef<SymbolCell> SymbolRef;
typedef StrongRef<InlineSymbolCell> InlineSymbolRef;
typedef StrongRef<HeapSymbolCell> HeapSymbolRef;
typedef StrongRef<NumberCell> NumberRef;
typedef StrongRef<ExactIntegerCell> ExactIntegerRef;
typedef StrongRef<FlonumCell> FlonumRef;
typedef StrongRef<CharCell> CharRef;
typedef StrongRef<VectorCell> VectorRef;
typedef StrongRef<BytevectorCell> BytevectorRef;
typedef StrongRef<RecordLikeCell> RecordLikeRef;
typedef StrongRef<ProcedureCell> ProcedureRef;
typedef StrongRef<RecordCell> RecordRef;
typedef StrongRef<ErrorObjectCell> ErrorObjectRef;
typedef StrongRef<PortCell> PortRef;
typedef StrongRef<MailboxCell> MailboxRef;
typedef StrongRef<DynamicStateCell> DynamicStateRef;

}
}

#endif
