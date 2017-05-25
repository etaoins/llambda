#include "binding/SharedByteArray.h"

#include <cstring>

#include "assertions.h"
#include "stubdefinitions.h"

using lliby::SharedByteArray;

int main(int argc, char *argv[])
{
	{
		SharedByteArray *handle1 = SharedByteArray::createUninitialised(5);

		// We should be exclusive
		ASSERT_TRUE(handle1->isExclusive());
		// We're not a shared constant
		ASSERT_FALSE(handle1->isSharedConstant());

		// Creating a writable reference should point to the same handle
		SharedByteArray *writable = handle1->asWritable(5);
		ASSERT_EQUAL(handle1, writable);

		// There is only one reference at this pointer
		// Unrefing it should delete it
		ASSERT_TRUE(handle1->unref());
	}

	{
		// Create a byte array
		SharedByteArray *handle1 = SharedByteArray::createUninitialised(5);
		// Create another reference to it
		SharedByteArray *handle2 = handle1->ref();

		// Handles should be pointing to the same array
		ASSERT_EQUAL(handle1, handle2);

		// We are no longer exclusive due to having two references
		ASSERT_FALSE(handle1->isExclusive());
		// We are stil not a shared constant
		ASSERT_FALSE(handle1->isExclusive());

		// Releasing the first handle shouldn't free
		ASSERT_FALSE(handle1->unref());
		// However, the second free should
		ASSERT_TRUE(handle2->unref());
	}

	{
		const char *testData = "01234";
		const std::size_t testDataSize = 5;

		// Create a byte array
		SharedByteArray *handle1 = SharedByteArray::createUninitialised(5);
		// Create another reference to it
		SharedByteArray *handle2 = handle1->ref();

		// Write some test values to the array
		memcpy(handle1->data(), testData, testDataSize);

		// Handles should be pointing to the same array at this point
		ASSERT_EQUAL(handle1, handle2);

		// Make handle2 writable
		handle2 = handle2->asWritable(testDataSize);

		// The handles should now be different
		ASSERT_TRUE(handle1 != handle2);

		// Both handles are now exclusive
		ASSERT_TRUE(handle1->isExclusive());
		ASSERT_TRUE(handle2->isExclusive());

		// Neither handle is a shared constant
		ASSERT_FALSE(handle1->isSharedConstant());
		ASSERT_FALSE(handle2->isSharedConstant());

		// Make sure both handles have the same data
		ASSERT_EQUAL(memcmp(handle1->data(), testData, testDataSize), 0);
		ASSERT_EQUAL(memcmp(handle2->data(), testData, testDataSize), 0);

		// Unrefing each handle should free it
		ASSERT_TRUE(handle1->unref());
		ASSERT_TRUE(handle2->unref());
	}

	{
		const std::size_t minimumByteCount = 17;

		SharedByteArray *handle1 = SharedByteArray::createUninitialised(minimumByteCount);

		ASSERT_TRUE(handle1->capacity(minimumByteCount) >= minimumByteCount);

		handle1->unref();
	}
}
