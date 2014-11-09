#include <sys/stat.h>

#include "binding/StringCell.h"

using namespace lliby;

extern "C"
{

bool lliby_file_exists(StringCell *filePath)
{
	struct stat statBuf;

	return stat(filePath->toUtf8StdString().c_str(), &statBuf) == 0;
}

}
