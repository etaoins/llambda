import re

from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def _type_name_to_underscore(string):
    return re.sub('([A-Z]+)', r'_\1', string).lower()

def generate_predicates(boxed_types):
    cxx_base_type = type_name_to_clike_class(BASE_TYPE)

    content  = GENERATED_FILE_COMMENT

    for type_name, boxed_type in boxed_types.items():
        cxx_type_name = type_name_to_clike_class(type_name)
        content += '#include "binding/' + cxx_type_name + '.h"\n'

    content += '\n'

    content += 'using namespace lliby;\n\n'

    content += 'extern "C"\n'
    content += '{\n\n'

    for type_name, boxed_type in boxed_types.items():
        if type_name == BASE_TYPE:
            # Doesn't make sense - every type is a subtype of the base type
            continue

        function_name = "lliby_is_" + _type_name_to_underscore(type_name)
        cxx_type_name = type_name_to_clike_class(type_name)

        content += 'bool ' + function_name + '(const ' + cxx_base_type + ' *value)\n'
        content += '{\n'
        content += '\treturn ' + cxx_type_name + '::isInstance(value);\n'
        content += '}\n\n'
    
    content += '\n'
    content += '}\n'

    return {'runtime/stdlib/generated/predicates.cpp': content}

