#ifndef _LLIBY_BINDING_TYPEDPROCEDURECELL_H
#define _LLIBY_BINDING_TYPEDPROCEDURECELL_H

#include "ProcedureCell.h"
#include "ReturnValuesList.h"

#include "alloc/allocator.h"

namespace lliby
{
class World;

/**
 * ProcedureCell with an explicitly known type
 *
 * As it is not possible to test a procedure's type during runtime its exact type must be explicitly known before the
 * procedure can be applied. Due to the fact our calling convention passes rest arguments as the final argument what may
 * be compatible types in Scheme are represented using distinct TypedProcedureCell types. For example, while (->
 * <number> <number> <number>) is a subtype of (-> <any> * <any>) they have different native signatures due to differing
 * number of fixed argumetns. For this reason the exact procedure type must be known and the normal subtyping rules
 * do not apply.
 *
 * If a (world-function) is declared in Scheme to take a certain function type the compiler will ensure that a
 * ProcedureCell with the corresponding typed entry point is generated and passed to the native function. Conversely,
 * if a (world-function) is defined to return a function type then the compiler will expect the native function to
 */
template<typename R, typename... Args>
class TypedProcedureCell : public ProcedureCell
{
public:
	/**
	 * Type of the entry point to this procedure
	 */
	using TypedEntryPoint = R (*)(World &, ProcedureCell*, Args...);
	
	/**
	 * Creates a new instance of this TypedProcedureCell
	 */
	static TypedProcedureCell<R, Args...>* createInstance(World &world, std::uint32_t recordClassId, bool dataIsInline, void *recordData, TypedEntryPoint entryPoint)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) TypedProcedureCell<R, Args...>(recordClassId, dataIsInline, recordData, entryPoint);
	}

	/**
	 * Applies this procedure with the given argument list
	 */
	R apply(World &world, Args... args)
	{ 
		auto castEntryPoint = reinterpret_cast<TypedEntryPoint>(entryPoint());
		return castEntryPoint(world, this, args...);
	}

	// Runtime testing of procedure cell subtypes is not possible
	static bool isInstance(const AnyCell *cell) = delete;
	
private:
	TypedProcedureCell<R, Args...>(std::uint32_t recordClassId, bool dataIsInline, void *recordData, TypedEntryPoint entryPoint) :
		ProcedureCell(recordClassId, dataIsInline, recordData, reinterpret_cast<void*>(entryPoint))
	{
	}
};

/**
 * TypedProcedureCell accepting an arbitrary number of arguments and producing arbitrary values
 *
 * This corresponds to the compiler's TopProcedureType and Scheme's <procedure> type. Note that this will only work
 * for procedures explicitly defined the top procedure type. See the TypedProcedureCell documentation for a discussion
 * of why subtyping doesn't work ask expected here.
 */
using TopProcedureCell = TypedProcedureCell<ReturnValuesList*, ListElementCell*>;

}

#endif
