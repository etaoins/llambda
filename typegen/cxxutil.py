import re

from typegen.exceptions import SemanticException
from typegen.clikeutil import *

def guard_cxx_header(content, guard_name):
    wrapped  = "#ifndef " + guard_name + "\n"
    wrapped += "#define " + guard_name + "\n\n"
    
    wrapped += GENERATED_FILE_COMMENT

    wrapped += content

    wrapped += "#endif\n"

    return wrapped

def _llvm_type_to_cxx(llvm_type, signed):
    # Are we a pointer?
    if llvm_type[-1:] == '*':
        # Recursively call ourselves with our base type
        return _llvm_type_to_cxx(llvm_type[:-1], signed) + '*'

    # Are we a struct?
    if llvm_type[0] == '%':
        return type_name_to_clike_class(llvm_type[1:])

    # These are the easy cases
    if llvm_type in ["double", "float"]:
        return llvm_type

    # Check if we're an integer
    integer_match = re.match("^i(\\d+)$", llvm_type)

    if integer_match:
        bit_width = integer_match.group(1)

        # Use the C++11 cstdint types
        if signed:
            return "std::int" + str(bit_width) + "_t"
        else:
            return "std::uint" + str(bit_width) + "_t"

    raise SemanticException('Cannot convert type "' + llvm_type + '"')

def _complex_type_to_cxx(complex_type):
    if complex_type == "bool":
        return "bool"
    elif complex_type == "entryPoint":
        # This is typedef'ed in declaretypes.h
        return "ProcedureEntryPoint"
    elif complex_type == "unicodeChar":
        return "UnicodeChar"
    else:
        raise SemanticException('Unknown complex type "' + complex_type + '"')

def field_type_to_cxx(field):
    if field.llvm_type:
        return _llvm_type_to_cxx(field.llvm_type, field.signed)
    else:
        return _complex_type_to_cxx(field.complex_type)

def type_name_to_enum_constant(type_name):
    return type_name[0].upper() + type_name[1:]
