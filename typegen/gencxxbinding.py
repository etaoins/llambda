from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *
from typegen.cxxutil import *

OUTPUT_DIR = "runtime/binding/generated/"

def _type_conditions_to_cxx(cell_type, datum_access = ""):
    conditions_cxx = []

    # Build the C++ for each condition
    for concrete_name, concrete_type in cell_type.concrete_types.items():
        type_name = concrete_type.name
        conditions_cxx.append("(" + datum_access + "typeId() == CellTypeId::" + type_name_to_enum_constant(concrete_name) + ")")

    # Join them with ||
    return " || ".join(conditions_cxx)

def _generate_typeid_enum(cell_types):
    # Find the C++ type of our type IDs
    typeid_type = field_type_to_cxx(cell_types[BASE_TYPE].fields["typeId"])
    
    content  = "#include <cstdint>\n\n"

    content += "namespace lliby\n" 
    content += "{\n\n"

    content += "enum class CellTypeId : " + typeid_type + "\n"
    content += "{\n"

    for type_name, cell_type in cell_types.items(): 
        type_id = cell_type.type_id
        if type_id is not None:
            cxx_name = type_name_to_enum_constant(type_name)
            content += "\t" + cxx_name + " = " + str(type_id) + ",\n"

    content += "};\n\n"

    content += "}\n\n"

    return guard_cxx_header(content, "_LLIBY_BINDING_TYPEID_H")

def _generate_declaretypes(cell_types):
    content  = "namespace lliby\n" 
    content += "{\n\n"

    for type_name, cell_type in cell_types.items():
        cxx_type_name = type_name_to_clike_class(type_name)
        content += "class " + cxx_type_name + ";\n"

    datum_class = type_name_to_clike_class(BASE_TYPE)
    list_element_class = type_name_to_clike_class('listElement')

    content += "typedef " + datum_class + "* (*ProcedureEntryPoint)(void*, " + list_element_class + "*);\n\n"

    content += "}\n\n"

    return guard_cxx_header(content, "_LLIBY_BINDING_DECLARETYPES_H")

def _generate_casts(cell_types, type_name):
    type_condition = _type_conditions_to_cxx(cell_types[type_name], "datum->")

    cxx_base_type_name = type_name_to_clike_class(BASE_TYPE)
    cxx_type_name = type_name_to_clike_class(type_name)

    content  = "\tstatic " + cxx_type_name + "* fromDatum(" + cxx_base_type_name + " *datum)\n" 
    content += "\t{\n"
    content += "\t\tif (" + type_condition + ")\n"
    content += "\t\t{\n"
    content += "\t\t\treturn reinterpret_cast<" + cxx_type_name + "*>(datum);\n"
    content += "\t\t}\n\n"
    content += "\t\treturn nullptr;\n"
    content += "\t}\n\n"
    
    content += "\tstatic const " + cxx_type_name + "* fromDatum(const " + cxx_base_type_name + " *datum)\n" 
    content += "\t{\n"
    content += "\t\tif (" + type_condition + ")\n"
    content += "\t\t{\n"
    content += "\t\t\treturn reinterpret_cast<const " + cxx_type_name + "*>(datum);\n"
    content += "\t\t}\n\n"
    content += "\t\treturn nullptr;\n"
    content += "\t}\n\n"
    
    content += "\tstatic bool isInstance(const " + cxx_base_type_name + " *datum)\n" 
    content += "\t{\n"
    content += "\t\treturn " + type_condition + ";\n"
    content += "\t}\n\n"

    return content


def _generate_type_members(cell_types, type_name):
    cell_type = cell_types[type_name]
    filename  = type_name_to_clike_class(cell_type.name) + 'Members'
    file_path = OUTPUT_DIR + filename + ".h"

    data_content = ""
    accessor_content = ""

    # Build the data and accessors in one pass 
    for field_name, field in cell_type.fields.items():
        member_name = "m_" + field_name

        if field.qualified_name == BASE_TYPE + ".typeId":
            member_type = "CellTypeId"
        elif field.qualified_name == BASE_TYPE + ".gcState":
            member_type = "GarbageState"
        else:
            member_type = field_type_to_cxx(field)

        data_content += "\t" + member_type + " " + member_name + ";\n"

        accessor_content += "\t" + member_type + " " + field_name + "() const\n"
        accessor_content += "\t{\n"
        accessor_content += "\t\treturn " + member_name + ";\n"
        accessor_content += "\t}\n\n"

    # Glue everything together in the right order
    content  = GENERATED_FILE_COMMENT

    if accessor_content:
        content += "public:\n"
        content += accessor_content

    if type_name != BASE_TYPE:
        content += "public:\n"
        content += _generate_casts(cell_types, type_name)

    if data_content:
        content += "private:\n"
        content += data_content

    return {file_path: content}

def generate_cxx_binding(cell_types):
    files = {}

    files[OUTPUT_DIR + "typeid.h"] = _generate_typeid_enum(cell_types)
    files[OUTPUT_DIR + "declaretypes.h"] = _generate_declaretypes(cell_types)

    for type_name, cell_type in cell_types.items():
        files.update(_generate_type_members(cell_types, type_name))

    return files
