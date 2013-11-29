import re

from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def _uppercase_first(string):
    return string[0].upper() + string[1:]

def _type_name_to_nfi_decl(type_name):
    return "boxed-" + re.sub('([A-Z][a-z]+)', r'-\1', type_name).lower()

def _llvm_type_integer_bits(llvm_type):
    if llvm_type is None:
        return None

    # Check if we're an integer
    integer_match = re.match("^i(\\d+)$", llvm_type)

    if integer_match:
        return int(integer_match.group(1))

    return None

def _llvm_type_to_scala(llvm_type):
    # Are we a pointer?
    if llvm_type[-1:] == '*':
        # Recursively call ourselves with our base type
        return 'PointerType(' +  _llvm_type_to_scala(llvm_type[:-1]) + ')'

    # Are we a struct?
    if llvm_type[0] == '%':
        return 'UserDefinedType("' + llvm_type[1:] + '")'

    # These are the easy cases
    if llvm_type == 'double':
        return "DoubleType"

    if llvm_type == 'float':
        return 'FloatType'

    # Check if we're an integer
    integer_bits = _llvm_type_integer_bits(llvm_type)

    if integer_bits:
        return 'IntegerType(' + str(integer_bits) + ')'

    raise SemanticException('Cannot convert type "' + llvm_type + '"')

def _complex_type_to_scala(complex_type):
    if complex_type == "bool":
        return 'IntegerType(8)'
    elif complex_type == "entryPoint":
        # Good lord
        return ('PointerType(' 
                  'FunctionType('
                    'PointerType(UserDefinedType("' + BASE_TYPE + '")), '
                    'List('
                      'PointerType(IntegerType(8)), '
                      'PointerType(UserDefinedType("listElement")) '
                    ')' 
                  ')'
                ')')

    elif complex_type == "unicodeChar":
        return _llvm_type_to_scala('i32')
    elif complex_type == "void*":
        return _llvm_type_to_scala('i8*')
    else:
        raise SemanticException('Unknown complex type "' + complex_type + '"')

def _field_type_to_scala(field):
    if field.llvm_type:
        return _llvm_type_to_scala(field.llvm_type)
    else:
        return _complex_type_to_scala(field.complex_type)

def _generate_field_types(current_type):
    supertype = current_type.supertype

    if supertype:
        output  = _generate_field_types(supertype)
    else:
        output = ''

    for field_name, field in current_type.fields.items():
        output += '  val ' + field_name + 'IrType = ' + _field_type_to_scala(field) + '\n'

    return output

def _recursive_fields(boxed_type):
    our_fields = boxed_type.fields.values()

    supertype = boxed_type.supertype
    if supertype:
        # gcState is always 0 for constants
        super_fields = [x for x in supertype.fields.values() if x.name != 'gcState']

        return super_fields + _recursive_fields(supertype)
    else:
        # No more super types
        return []

def _generate_constant_constructor(all_types, boxed_type):
    constant_type_id = boxed_type.type_id

    our_fields = list(boxed_type.fields.values())
    recursive_fields = _recursive_fields(boxed_type)

    # Determine our parameters
    parameter_defs = []

    for field in our_fields + recursive_fields:
        if field.name == 'gcState':
            # This is always 0 for constants
            continue

        if (field.name == 'typeId') and constant_type_id is not None:
            # We know our own type ID
            continue


        # See if this is an integer field
        field_int_bits = _llvm_type_integer_bits(field.llvm_type)

        if field_int_bits:
            # Take an integer directly
            param_type = 'Long'
        else:
            # Require a llvmir.IrConstant
            param_type = 'IrConstant'


        parameter_defs.append(field.name + ' : ' + param_type)

    output = '  def createConstant('
    output += ', '.join(parameter_defs) + ') : StructureConstant = {\n'

    for field_name, field in boxed_type.fields.items():
        if field_name == 'gcState':
            # These are provided internally
            continue

        if _llvm_type_integer_bits(field.llvm_type):
            # We generate the IrContants ourselces
            continue

        exception_message = "Unexpected type for field " + field_name

        output += '    if (' + field_name + '.irType != ' + field_name + 'IrType) {\n'
        output += '      throw new InternalCompilerErrorException("' + exception_message + '")\n'
        output += '    }\n\n'

    output += '    StructureConstant(List(\n'

    supertype = boxed_type.supertype
    if supertype:
        output += '      supertype.get.createConstant(\n'

        super_parameters = []

        for field in recursive_fields:
            if (field.name == 'typeId') and (constant_type_id is not None):
                super_parameters.append('        typeId=typeId')
            else:
                super_parameters.append('        ' + field.name + '=' + field.name)

        output += ",\n".join(super_parameters) + "\n"

        if len(our_fields):
            output += '      ),\n'
        else:
            output += '      )'

    our_field_values = []
    for field in our_fields:
        if field.name == 'gcState':
            our_field_values.append('      IntegerConstant(gcStateIrType, 0)')
        else:
            field_int_bits = _llvm_type_integer_bits(field.llvm_type)

            if field_int_bits:
                # Wrap the integer for them
                our_field_values.append('      IntegerConstant(' + field.name + 'IrType, ' + field.name + ')')
            else:
                our_field_values.append('      ' + field.name)

    output += ",\n".join(our_field_values) + "\n"

    output += '    ), userDefinedType=Some(irType))\n'
    output += '  }\n'

    return output

def _generate_field_accessors(leaf_type, current_type, declare_only = False, depth = 1):
    supertype = current_type.supertype

    if supertype:
        output  = _generate_field_accessors(leaf_type, supertype, declare_only, depth + 1)

        # Our supertype is our first "field"
        field_counter = 1
    else:
        output = ''
        field_counter = 0

    for field_name, field in current_type.fields.items():
        uppercase_field_name = _uppercase_first(field_name)

        pointer_method_name = 'genPointerTo' + uppercase_field_name
        store_method_name = 'genStoreTo' + uppercase_field_name
        load_method_name = 'genLoadFrom' + uppercase_field_name

        output += '\n'

        output += '  def ' + pointer_method_name + '(block : IrBlockBuilder)(boxedValue : IrValue) : IrValue'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'
            
            # Make sure we're the correct type
            exception_message = "Unexpected type for boxed value. Passed ${boxedValue.irType}, expected %" + leaf_type.name + "*"
            output += '    if (boxedValue.irType != PointerType(UserDefinedType("' + leaf_type.name + '"))) {\n'
            output += '       throw new InternalCompilerErrorException(s"' + exception_message + '")\n'
            output += '    }\n\n'

            # Calculate or indices
            indices = map(str, ([0] * depth) + [field_counter])
            field_counter = field_counter + 1

            output += '    block.getelementptr("'+ field_name + 'Ptr")(\n'
            output += '      elementType=' + field_name + 'IrType,\n'
            output += '      basePointer=boxedValue,\n'
            output += '      indices=List(' + ", ".join(indices) + ').map(IntegerConstant(IntegerType(32), _)),\n'
            output += '      inbounds=true\n'
            output += '    )\n'
            output += '  }\n'
            output += '\n'
        
        output += '  def ' + store_method_name  + '(block : IrBlockBuilder)(toStore : IrValue, boxedValue : IrValue) : Unit'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'

            pointer_name = field_name + 'Pointer'
            output += '    val ' + pointer_name + ' = ' + pointer_method_name + '(block)(boxedValue)\n'
            output += '    block.store(toStore, ' + pointer_name + ', tbaaIndex=Some(tbaaIndex))\n' 
            output += '  }\n'
            output += '\n'
        
        output += '  def ' + load_method_name  + '(block : IrBlockBuilder)(boxedValue : IrValue) : IrValue'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'

            pointer_name = field_name + 'Pointer'
            output += '    val ' + pointer_name + ' = ' + pointer_method_name + '(block)(boxedValue)\n'
            output += '    block.load("' + field_name + '")(' + pointer_name + ', tbaaIndex=Some(tbaaIndex))\n' 
            output += '  }\n'

    return output

def _generate_unconditional_type_check():
    output  = '  def genTypeCheck(startBlock : IrBlockBuilder)(boxedValue : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {\n'
    output += '    startBlock.uncondBranch(successBlock)\n'
    output += '  }\n'

    return output

def _generate_type_check(all_types, boxed_type):
    base_type_object = type_name_to_clike_class(BASE_TYPE)

    output  = '  def genTypeCheck(startBlock : IrBlockBuilder)(boxedValue : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {\n'
    output += '    val datumValue = ' + base_type_object + '.genPointerBitcast(startBlock)(boxedValue)\n'
    output += '    val typeId = BoxedDatum.genLoadFromTypeId(startBlock)(datumValue)\n'

    switch_params = []

    # Switch on the type ID
    switch_params.append("typeId")

    # If the type ID isn't known then fail the branch
    switch_params.append("failBlock")

    for concrete_type in boxed_type.concrete_types.values():
        switch_params.append("(" + str(concrete_type.type_id) + "L -> successBlock)")
        
    output += '    startBlock.switch(' + ", ".join(switch_params) + ')\n'
    output += '  }\n'

    return output

def _generate_boxed_types(all_types):
    base_type = all_types[BASE_TYPE]

    output  = GENERATED_FILE_COMMENT
    
    output += "package llambda.boxedtype\n\n"

    output += "import llambda.codegen.llvmir._\n"
    output += "import llambda.InternalCompilerErrorException\n\n"
        
    output += 'sealed abstract class BoxedType {\n'
    output += '  val name : String\n'
    output += '  val irType : FirstClassType\n'
    output += '  val supertype : Option[BoxedType]\n'
    output += '  val directSubtypes : Set[BoxedType]\n'
    output += '  val isAbstract : Boolean\n' 
    output += '  val tbaaIndex : Int\n' 
    output += '\n'
    output += '  def genTypeCheck(startBlock : IrBlockBuilder)(boxedValue : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget)\n'
    output += '\n'
    output += '  def isTypeOrSubtypeOf(otherType : BoxedType) : Boolean = {\n'
    output += '    if (otherType == this) {\n'
    output += '      return true\n'
    output += '    }\n'
    output += '\n'
    output += '    supertype map (_.isTypeOrSubtypeOf(otherType)) getOrElse false\n'
    output += '  }\n'
    output += '\n'
    output += '  def isTypeOrSupertypeOf(otherType : BoxedType) : Boolean = {\n'
    output += '    if (otherType == this) {\n'
    output += '      return true\n'
    output += '    }\n'
    output += '\n'
    output += '    directSubtypes exists (_.isTypeOrSupertypeOf(otherType))\n'
    output += '  }\n'
    output += '\n'
    output += '  def concreteTypes : Set[ConcreteBoxedType] = this match {\n'
    output += '    case concreteType : ConcreteBoxedType => Set(concreteType)\n'
    output += '    case abstractType => directSubtypes.flatMap(_.concreteTypes)\n'
    output += '  }\n'
    output += '\n'
    output += '  def genPointerBitcast(block : IrBlockBuilder)(uncastValue : IrValue) : IrValue =\n'
    output += '    if (uncastValue.irType == PointerType(irType)) {\n'
    output += '      uncastValue\n'
    output += '    }\n'
    output += '    else {\n'
    output += '      block.bitcastTo(name + "Cast")(uncastValue, PointerType(irType))\n'
    output += '    }\n'
    output += _generate_field_accessors(base_type, base_type, True)
    output += '}\n\n'

    output += 'sealed abstract class ConcreteBoxedType extends BoxedType {\n'
    output += '  val typeId : Int\n'
    output += '}\n\n'

    for type_name, boxed_type in all_types.items():
        object_name = type_name_to_clike_class(type_name)
        supertype_name = boxed_type.inherits

        if boxed_type.abstract:
            scala_superclass = 'BoxedType'
        else:
            scala_superclass = 'ConcreteBoxedType'

        output += 'object ' + object_name + ' extends ' + scala_superclass + ' {\n'
        output += '  val name = "' + type_name + '"\n'
        output += '  val irType = UserDefinedType("' + type_name + '")\n'

        if supertype_name:
            supertype_object = type_name_to_clike_class(supertype_name)
            output += '  val supertype = Some(' + supertype_object + ')\n'
        else:
            output += '  val supertype = None\n'

        subtype_scala_names = []
        for subtype_name in boxed_type.subtypes.keys():
            subtype_object = type_name_to_clike_class(subtype_name)
            subtype_scala_names.append(subtype_object)

        output += '  val directSubtypes = Set[BoxedType](' + ", ".join(subtype_scala_names) + ')\n'

        if boxed_type.abstract:
            output += '  val isAbstract = true\n'
        else:
            output += '  val isAbstract = false\n'

        output += '  val tbaaIndex = ' + str(boxed_type.index) + '\n'

        type_id = boxed_type.type_id
        if type_id is not None:
            output += '  val typeId = ' + str(type_id) + '\n'
        output += "\n"
        
        output += _generate_field_types(boxed_type)

        if not boxed_type.singleton:
            output += '\n'
            output += _generate_constant_constructor(all_types, boxed_type)
        
        output += '\n'

        if type_name == BASE_TYPE:
            # Every type should be an instance of the base type; generate a 
            # stub type check that always passes
            output += _generate_unconditional_type_check()
        else:
            output += _generate_type_check(all_types, boxed_type)
        
        output += _generate_field_accessors(boxed_type, boxed_type)

        output += '}\n\n'

    return output

def _generate_name_to_boxed_type(all_types):
    output  = GENERATED_FILE_COMMENT
    output += 'package llambda.frontend\n\n'

    output += 'import llambda.{boxedtype => bt}\n\n'

    output += 'object IntrinsicBoxedTypes {\n'
    output += '  def apply() : Map[String, bt.BoxedType] = Map(\n'

    map_members = []
    for type_name, boxed_type in all_types.items():
        if boxed_type.internal:
            # This isn't meant to be exposed as an NFI type
            continue

        nfi_decl_name = _type_name_to_nfi_decl(type_name)
        boxed_type_class = type_name_to_clike_class(type_name)

        map_members.append('    ("<' + nfi_decl_name + '>" -> ' + 'bt.' + boxed_type_class + ')')

    output += ',\n'.join(map_members) + '\n'

    output += '  )\n'
    output += '}\n'
    return output

def generate_scala_objects(all_types):
    ROOT_PATH = 'compiler/src/main/scala/llambda/'

    return {ROOT_PATH + 'boxedtype/generated/BoxedType.scala': _generate_boxed_types(all_types),
            ROOT_PATH + 'frontend/generated/IntrinsicBoxedTypes.scala': _generate_name_to_boxed_type(all_types)}
