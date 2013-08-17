import re

from typegen.constants import BASE_TYPE
from typegen.cxxutil import *

def _type_name_to_underscore(string):
    return re.sub('([A-Z]+)', r'_\1', string).lower()

def generate_predicates(boxed_types):
    cxx_base_type = type_name_to_cxx(BASE_TYPE)

    content  = GENERATED_FILE_COMMENT

    content += '#include "binding/' + cxx_base_type + '.h"\n\n'

    content += 'using namespace lliby;\n\n'

    content += 'extern "C"\n'
    content += '{\n\n'

    for type_name, boxed_type in boxed_types.items():
        if boxed_type.type_id is None:
            # Can't check types without a type ID
            continue

        function_name = "lliby_is_" + _type_name_to_underscore(type_name)
        cxx_type_name = type_name_to_cxx(type_name)

        content += 'bool ' + function_name + '(const ' + cxx_base_type + ' *value)\n'
        content += '{\n'
        content += '\treturn value->as' + cxx_type_name + '() != nullptr;\n'
        content += '}\n\n'
    
    content += '\n'
    content += '}\n'

    return {'runtime/stdlib/generated/predicates.cpp': content}
