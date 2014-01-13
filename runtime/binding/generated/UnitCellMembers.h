/************************************************************
 * This file is generated by typegen. Do not edit manually. *
 ************************************************************/

public:
	static bool isInstance(const DatumCell *datum)
	{
		return datum->typeId() == CellTypeId::Unit;
	}

	static UnitCell* fromDatum(DatumCell *datum)
	{
		if (isInstance(datum))
		{
			return static_cast<UnitCell*>(datum);
		}

		return nullptr;
	}

	static const UnitCell* fromDatum(const DatumCell *datum)
	{
		if (isInstance(datum))
		{
			return static_cast<const UnitCell*>(datum);
		}

		return nullptr;
	}
