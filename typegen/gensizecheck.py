import re

from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def generate_size_check(cell_types):
    cxx_base_type = type_name_to_clike_class(BASE_TYPE)

    content  = GENERATED_FILE_COMMENT

    for type_name, cell_type in cell_types.items():
        cxx_type_name = type_name_to_clike_class(type_name)

        if not cell_type.abstract:
            content += '#include "binding/' + cxx_type_name + '.h"\n'

    content += '\n'

    for type_name, cell_type in cell_types.items():
        # Abstract types can't be directly allocated so don't check
        if type_name == BASE_TYPE or cell_type.abstract:
            continue

        cxx_type_name = type_name_to_clike_class(type_name)
        # This assumes "Cons" is our allocation size
        messsage = cxx_type_name + " does not fit in to a Cons cell"

        content += 'static_assert(sizeof(lliby::' + cxx_type_name + ') <= sizeof(lliby::alloc::Cons), "' + messsage + '");\n'

    return {'runtime/alloc/generated/sizecheck.h': content}

