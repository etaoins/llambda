#include <clocale>

extern "C"
{

void lliby_init()
{
	// Use the user preferred locale
	// We assume a UTF-8 locale but don't explicitly set "UTF-8" so we still
	// get user-defined string sorting etc.
	std::setlocale(LC_ALL, "");
}

}
