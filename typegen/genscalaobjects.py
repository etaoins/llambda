import re

from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.boxedtype import TypeEqualsCondition, TypeBitmaskCondition
from typegen.clikeutil import *

def _type_name_to_nfi_decl(type_name):
    return "boxed-" + re.sub('([A-Z][a-z]+)', r'-\1', type_name).lower()

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

def _recursive_field_names(boxed_type):
    our_fields = boxed_type.fields.keys()

    supertype = boxed_type.supertype
    if supertype:
        # gcState is always 0 for constants
        super_fields = [x for x in supertype.fields.keys() if x != 'gcState']

        return super_fields + _recursive_field_names(supertype)
    else:
        # No more super types
        return []

def _generate_constant_constructor(boxed_types, boxed_type):
    constant_type_id = boxed_type.type_id

    our_field_names = list(boxed_type.fields.keys())
    recursive_field_names = _recursive_field_names(boxed_type)

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

    supertype = boxed_type.supertype
    if supertype:
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

def _generate_field_accessors(leaf_type, current_type, depth = 1):
    field_counter = 0

    output = ''
    for field_name, field in current_type.fields.items():
        accessor_name = field_name[0].upper() + field_name[1:]

        output += '\n'
        output += '  def pointerTo' + accessor_name + '(block : IrBlockBuilder, boxedValue : IrValue) : IrValue = {\n'
        
        # Make sure we're the correct type
        exception_message = "Unexpected type for boxed value"
        output += '    if (boxedValue.irType != UserDefinedType("' + leaf_type.name + '")) {\n'
        output += '       throw new InternalCompilerErrorException("' + exception_message + '")\n'
        output += '    }\n\n'

        # Calculate or indices
        indices = map(str, ([0] * depth) + [field_counter])
        field_counter = field_counter + 1

        output += '    block.getelementptr("'+ field_name + '")(\n'
        output += '      resultType=' + _field_type_to_scala(field) + ',\n'
        output += '      basePointer=boxedValue,\n'
        output += '      indices=List(' + ", ".join(indices) + ').map(IntegerConstant(IntegerType(32), _)),\n'
        output += '      inbounds=true\n'
        output += '    )\n'
        output += '  }\n'

    supertype = current_type.supertype
    if supertype:
        output += _generate_field_accessors(leaf_type, supertype, depth + 1)

    return output

def _generate_boxed_types(boxed_types):
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
        supertype_name = boxed_type.inherits

        output += 'object ' + object_name + ' extends BoxedType {\n'
        output += '  val irType = UserDefinedType("' + type_name + '")\n'

        if supertype_name:
            supertype_object = type_name_to_clike_class(supertype_name)
            output += '  val superType = Some(' + supertype_object + ')\n'
        else:
            output += '  val superType = None\n'

        type_id = boxed_type.type_id
        if type_id is not None:
            output += '  val typeId = ' + str(type_id) + '\n'

        if not boxed_type.singleton:
            output += '\n'
            output += _generate_constant_constructor(boxed_types, boxed_type)
        
        output += _generate_field_accessors(boxed_type, boxed_type)

        output += '}\n\n'


    return output

def _generate_name_to_boxed_type(boxed_types):
    output  = GENERATED_FILE_COMMENT
    output += 'package llambda.frontend\n\n'

    output += 'import llambda.codegen.{boxedtype => bt}\n\n'

    output += 'object NativeTypeNameToBoxedType {\n'
    output += '  def apply : PartialFunction[String, bt.BoxedType] = {\n'

    for type_name, boxed_type in boxed_types.items():
        nfi_decl_name = _type_name_to_nfi_decl(type_name)
        boxed_type_class = type_name_to_clike_class(type_name)

        output += '    case "' + nfi_decl_name + '" => '
        output += 'bt.' + boxed_type_class + '\n'

    output += '  }\n'
    output += '}\n'
    return output

def generate_scala_objects(boxed_types):
    ROOT_PATH = 'compiler/src/main/scala/llambda/'

    return {ROOT_PATH + 'codegen/generated/BoxedType.scala': _generate_boxed_types(boxed_types),
            ROOT_PATH + 'frontend/generated/NativeTypeNameToBoxedType.scala': _generate_name_to_boxed_type(boxed_types)}
