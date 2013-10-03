from typegen.exceptions import SemanticException
from typegen.boxedtype import TypeEqualsCondition, TypeBitmaskCondition
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *
from typegen.cxxutil import *

OUTPUT_DIR = "runtime/binding/generated/"

def _type_conditions_to_cxx(conditions):
    conditions_cxx = []

    # Build the C++ for each condition
    for condition in conditions:
        if isinstance(condition, TypeEqualsCondition):
            type_name = condition.boxed_type.name
            conditions_cxx.append("(typeId() == BoxedTypeId::" + type_name_to_enum_constant(type_name) + ")")
        elif isinstance(condition, TypeBitmaskCondition):
            conditions_cxx.append("(static_cast<int>(typeId()) & " + hex(condition.bitmask) + ")")
        else:
            raise SemanticException("Unknown condition type " + condition.__name__)

    # Join them with ||
    return " || ".join(conditions_cxx)

def _generate_typeid_enum(boxed_types):
    # Find the C++ type of our type IDs
    typeid_type = field_type_to_cxx(boxed_types[BASE_TYPE].fields["typeId"])
    
    content  = "#include <cstdint>\n\n"

    content += "namespace lliby\n" 
    content += "{\n\n"

    content += "enum class BoxedTypeId : " + typeid_type + "\n"
    content += "{\n"

    for type_name, boxed_type in boxed_types.items(): 
        type_id = boxed_type.type_id
        if type_id is not None:
            cxx_name = type_name_to_enum_constant(type_name)
            content += "\t" + cxx_name + " = " + str(type_id) + ",\n"

    content += "};\n\n"

    content += "}\n\n"

    return guard_cxx_header(content, "_LLIBY_BINDING_TYPEID_H")

def _generate_declaretypes(boxed_types):
    content  = "namespace lliby\n" 
    content += "{\n\n"

    for type_name, boxed_type in boxed_types.items():
        cxx_type_name = type_name_to_clike_class(type_name)
        content += "class " + cxx_type_name + ";\n"

    datum_class = type_name_to_clike_class(BASE_TYPE)
    procedure_class = type_name_to_clike_class('procedure')
    list_element_class = type_name_to_clike_class('listElement')

    content += "typedef " + datum_class + "* (*ProcedureEntryPoint)(" + procedure_class + "*, " + list_element_class + "*);\n\n"

    content += "}\n\n"

    return guard_cxx_header(content, "_LLIBY_BINDING_DECLARETYPES_H")

def _generate_casts(boxed_types):
    content = ""

    for type_name, boxed_type in boxed_types.items():
        if not boxed_type.type_conditions:
            continue
    
        if type_name == BASE_TYPE:
            # Doesn't make sense - every type is a subtype of the base type
            continue

        # Build our type condition
        type_condition = _type_conditions_to_cxx(boxed_type.type_conditions)
        cxx_type_name = type_name_to_clike_class(type_name)
        
        content += "\t" + cxx_type_name + "* as" + cxx_type_name + "()\n" 
        content += "\t{\n"
        content += "\t\tif (" + type_condition + ")\n"
        content += "\t\t{\n"
        content += "\t\t\treturn reinterpret_cast<" + cxx_type_name + "*>(this);\n"
        content += "\t\t}\n\n"
        content += "\t\treturn nullptr;\n"
        content += "\t}\n\n"
        
        content += "\tconst " + cxx_type_name + "* as" + cxx_type_name + "() const\n" 
        content += "\t{\n"
        content += "\t\tif (" + type_condition + ")\n"
        content += "\t\t{\n"
        content += "\t\t\treturn reinterpret_cast<const " + cxx_type_name + "*>(this);\n"
        content += "\t\t}\n\n"
        content += "\t\treturn nullptr;\n"
        content += "\t}\n\n"
        
        content += "\tbool is" + cxx_type_name + "() const\n" 
        content += "\t{\n"
        content += "\t\treturn " + type_condition + ";\n"
        content += "\t}\n\n"

    return content


def _generate_type_members(boxed_types, type_name):
    boxed_type = boxed_types[type_name]
    base_name  = type_name_to_clike_class(boxed_type.name) + 'Members'
    filename = OUTPUT_DIR + base_name + ".h"

    data_content = ""
    accessor_content = ""

    # Build the data and accessors in one pass 
    for field_name, field in boxed_type.fields.items():
        member_name = "m_" + field_name

        if field.qualified_name == BASE_TYPE + ".typeId":
            member_type = "BoxedTypeId"
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

    if (type_name == BASE_TYPE):
        content += "public:\n"
        content += _generate_casts(boxed_types)

    if data_content:
        content += "private:\n"
        content += data_content

    return {filename: content}

def generate_cxx_binding(boxed_types):
    files = {}

    files[OUTPUT_DIR + "typeid.h"] = _generate_typeid_enum(boxed_types)
    files[OUTPUT_DIR + "declaretypes.h"] = _generate_declaretypes(boxed_types)

    for type_name, boxed_type in boxed_types.items():
        files.update(_generate_type_members(boxed_types, type_name))

    return files
