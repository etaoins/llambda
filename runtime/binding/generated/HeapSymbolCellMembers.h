/************************************************************
 * This file is generated by typegen. Do not edit manually. *
 ************************************************************/

public:
	std::uint16_t charLength() const
	{
		return m_charLength;
	}

	SharedByteArray* heapByteArray() const
	{
		return m_heapByteArray;
	}

private:
	std::uint16_t m_charLength;
	SharedByteArray* m_heapByteArray;
