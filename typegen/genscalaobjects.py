import re

from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.boxedtype import TypeEqualsAssertion, TypeBitmaskAssertion, TypeUnionAssertion 
from typegen.clikeutil import *

def _llvm_type_to_scala(llvm_type, signed):
    # Are we a pointer?
    if llvm_type[-1:] == '*':
        # Recursively call ourselves with our base type
        return 'PointerType(' +  _llvm_type_to_scala(llvm_type[:-1], signed) + ')'

    # Are we a struct?
    if llvm_type[0] == '%':
        return 'UserDefinedType("' + llvm_type[1:] + '")'

    # These are the easy cases
    if llvm_type == 'double':
        return "DoubleType"

    if llvm_type == 'float':
        return 'SingleType'

    # Check if we're an integer
    integer_match = re.match("^i(\\d+)$", llvm_type)

    if integer_match:
        bit_width = integer_match.group(1)
        return 'IntegerType(' + str(bit_width) + ')'

    raise SemanticException('Cannot convert type "' + llvm_type + '"')

def _complex_type_to_scala(complex_type):
    if complex_type == "bool":
        return 'IntegerType(1)'
    elif complex_type == "entryPoint":
        # Good lord
        return ('PointerType(' 
                  'FunctionType('
                    'PointerType(UserDefinedType("' + BASE_TYPE + '")), '
                    'List('
                      'PointerType(UserDefinedType("closure")), '
                      'PointerType(UserDefinedType("' + BASE_TYPE + '")) '
                    ')' 
                  ')'
                ')')

    elif complex_type == "unicodeChar":
        return 'IntegerType(32)'
    else:
        raise SemanticException('Unknown complex type "' + complex_type + '"')

def _field_type_to_scala(field):
    if field.llvm_type:
        return _llvm_type_to_scala(field.llvm_type, field.signed)
    else:
        return _complex_type_to_scala(field.complex_type)

def _recursive_field_names(boxed_types, boxed_type):
    our_fields = boxed_type.fields.keys()

    if boxed_type.inherits:
        super_type = boxed_types[boxed_type.inherits]

        # gcState is always 0 for constants
        super_fields = [x for x in super_type.fields.keys() if x != 'gcState']

        return super_fields + _recursive_field_names(boxed_types, super_type)
    else:
        # No more super types
        return []

def _generate_constant_constructor(boxed_types, boxed_type):
    # Are we fully concrete?
    assertion = boxed_type.type_assertion
    if isinstance(assertion, TypeEqualsAssertion):
        constant_type_id = assertion.type_id_value
    else:
        constant_type_id = None

    our_field_names = list(boxed_type.fields.keys())
    recursive_field_names = _recursive_field_names(boxed_types, boxed_type)

    # Determine our parameters
    parameter_defs = []

    for field_name in our_field_names + recursive_field_names:
        if field_name == 'gcState':
            # This is always 0 for constants
            continue

        if (field_name == 'typeId') and constant_type_id is not None:
            # We know our own type ID
            continue

        parameter_defs.append(field_name + ' : IrConstant')

    output = '  def createConstant('
    output += ', '.join(parameter_defs) + ') : StructureConstant = {\n'

    for field_name, field in boxed_type.fields.items():
        if field_name == 'gcState':
            # These are provided internally
            continue

        expected_type = _field_type_to_scala(field)
        exception_message = "Unexpected type for field " + field_name

        output += '    if (' + field_name + '.irType != ' + expected_type + ') {\n'
        output += '       throw new InternalCompilerErrorException("' + exception_message + '")\n'
        output += '    }\n\n'

    output += '    StructureConstant(List(\n'

    if boxed_type.inherits:
        super_type = boxed_types[boxed_type.inherits]

        output += '      superType.get.createConstant(\n'

        super_parameters = []

        for field_name in recursive_field_names:
            if (field_name == 'typeId') and (constant_type_id is not None):
                type_id_field = boxed_types[BASE_TYPE].fields['typeId']
                type_id_type = _field_type_to_scala(type_id_field)
                type_id_constant = 'IntegerConstant(' + type_id_type + ', typeId)'                
                super_parameters.append('        typeId=' + type_id_constant)
            else:
                super_parameters.append('        ' + field_name + '=' + field_name)

        output += ",\n".join(super_parameters) + "\n"

        if len(our_field_names):
            output += '      ),\n'
        else:
            output += '      )'

    our_fields = []
    for field_name in our_field_names:
        if field_name == 'gcState':
            gcstate_field = boxed_types[BASE_TYPE].fields['gcState']
            gcstate_type = _field_type_to_scala(gcstate_field)
            
            our_fields.append('      IntegerConstant(' + gcstate_type + ', 0)')
        else:
            our_fields.append('      ' + field_name)

    output += ",\n".join(our_fields) + "\n"

    output += '    ), userDefinedType=Some(irType))\n'
    output += '  }\n'

    return output


def generate_scala_objects(boxed_types):
    output  = GENERATED_FILE_COMMENT
    
    output += "package llambda.codegen.boxedtype\n\n"

    output += "import llambda.codegen.llvmir._\n"
    output += "import llambda.InternalCompilerErrorException\n\n"
        
    output += 'sealed abstract class BoxedType {\n'
    output += '  val irType : FirstClassType\n'
    output += '  val superType : Option[BoxedType]\n'
    output += '}\n\n'

    for type_name, boxed_type in boxed_types.items():
        object_name = type_name_to_clike_class(type_name)
        super_type_name = boxed_type.inherits

        output += 'object ' + object_name + ' extends BoxedType {\n'
        output += '  val irType = UserDefinedType("' + type_name + '")\n'

        if super_type_name:
            super_type_object = type_name_to_clike_class(super_type_name)
            output += '  val superType = Some(' + super_type_object + ')\n'
        else:
            output += '  val superType = None\n'

        assertion = boxed_type.type_assertion
        if isinstance(assertion, TypeEqualsAssertion):
            output += '  val typeId = ' + str(assertion.type_id_value) + '\n'

        output += '\n'
        output += _generate_constant_constructor(boxed_types, boxed_type)

        output += '}\n\n'

    return {'compiler/src/main/scala/llambda/codegen/generated/BoxedType.scala': output}
