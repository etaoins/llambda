/************************************************************
 * This file is generated by typegen. Do not edit manually. *
 ************************************************************/

public:
	static bool typeIdIsTypeOrSubtype(CellTypeId typeId)
	{
		return typeId == CellTypeId::EofObject;
	}

	static bool isInstance(const AnyCell *cell)
	{
		return typeIdIsTypeOrSubtype(cell->typeId());
	}
