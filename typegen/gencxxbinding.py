from typegen.exceptions import SemanticException
from typegen.boxedtype import TypeEqualsAssertion, TypeBitmaskAssertion, TypeUnionAssertion 
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *
from typegen.cxxutil import *

OUTPUT_DIR = "runtime/binding/generated/"

def _type_assertion_to_cxx(assertion):
    if isinstance(assertion, TypeEqualsAssertion):
        type_name = assertion.boxed_type.name
        return "typeId() == BoxedTypeId::" + type_name_to_enum_constant(type_name)
    elif isinstance(assertion, TypeBitmaskAssertion):
        return "static_cast<int>(typeId()) & " + assertion.bitmask
    elif isinstance(assertion, TypeUnionAssertion):
        # Convert our subassertions to C++
        subassertions_cxx = []

        for subassertion in assertion.subassertions:
            subassertion_cxx = "(" + _type_assertion_to_cxx(subassertion) + ")"
            subassertions_cxx.append(subassertion_cxx)

        # Join them with ||
        return " || ".join(subassertions_cxx)
    else:
        raise SemanticException("Unknown assertion type " + assertion.__name__)

def _generate_typeid_enum(boxed_types):
    # Find the C++ type of our type IDs
    typeid_type = field_type_to_cxx(boxed_types[BASE_TYPE].fields["typeId"])
    
    content  = "#include <cstdint>\n\n"

    content += "namespace lliby\n" 
    content += "{\n\n"

    content += "enum class BoxedTypeId : " + typeid_type + "\n"
    content += "{\n"

    for type_name, boxed_type in boxed_types.items(): 
        if isinstance(boxed_type.type_assertion, TypeEqualsAssertion):
            cxx_name = type_name_to_enum_constant(type_name)
            type_id = boxed_type.type_assertion.type_id_value
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

    content += "typedef BoxedDatum* (*ProcedureEntryPoint)(ClosureValue*, BoxedDatum*);\n\n"

    content += "}\n\n"

    return guard_cxx_header(content, "_LLIBY_BINDING_DECLARETYPES_H")

def _generate_casts(boxed_types):
    content = ""

    for type_name, boxed_type in boxed_types.items():
        if boxed_type.type_assertion is None:
            continue
    
        if type_name == BASE_TYPE:
            # Doesn't make sense - every type is a subtype of the base type
            continue

        # Build our type assertion
        type_assertion = _type_assertion_to_cxx(boxed_type.type_assertion)
        cxx_type_name = type_name_to_clike_class(type_name)
        
        content += "\t" + cxx_type_name + "* as" + cxx_type_name + "()\n" 
        content += "\t{\n"
        content += "\t\tif (" + type_assertion + ")\n"
        content += "\t\t{\n"
        content += "\t\t\treturn reinterpret_cast<" + cxx_type_name + "*>(this);\n"
        content += "\t\t}\n\n"
        content += "\t\treturn nullptr;\n"
        content += "\t}\n\n"
        
        content += "\tconst " + cxx_type_name + "* as" + cxx_type_name + "() const\n" 
        content += "\t{\n"
        content += "\t\tif (" + type_assertion + ")\n"
        content += "\t\t{\n"
        content += "\t\t\treturn reinterpret_cast<const " + cxx_type_name + "*>(this);\n"
        content += "\t\t}\n\n"
        content += "\t\treturn nullptr;\n"
        content += "\t}\n\n"
        
        content += "\tbool is" + cxx_type_name + "() const\n" 
        content += "\t{\n"
        content += "\t\treturn " + type_assertion + ";\n"
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
