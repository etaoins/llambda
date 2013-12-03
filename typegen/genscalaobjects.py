import re

from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def _uppercase_first(string):
    return string[0].upper() + string[1:]

def _type_name_to_nfi_decl(type_name):
    return re.sub('([A-Z][a-z]+)', r'-\1', type_name).lower() + "-cell"

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

def _recursive_fields(cell_type):
    our_fields = cell_type.fields.values()

    supertype = cell_type.supertype
    if supertype:
        # gcState is always 0 for constants
        super_fields = [x for x in supertype.fields.values() if x.name != 'gcState']

        return super_fields + _recursive_fields(supertype)
    else:
        # No more super types
        return []

def _generate_constant_constructor(all_types, cell_type):
    constant_type_id = cell_type.type_id

    our_fields = list(cell_type.fields.values())
    recursive_fields = _recursive_fields(cell_type)

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

    for field_name, field in cell_type.fields.items():
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

    supertype = cell_type.supertype
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

        output += '  def ' + pointer_method_name + '(block : IrBlockBuilder)(valueCell : IrValue) : IrValue'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'
            
            # Make sure we're the correct type
            exception_message = "Unexpected type for cell value. Passed ${valueCell.irType}, expected %" + leaf_type.name + "*"
            output += '    if (valueCell.irType != PointerType(UserDefinedType("' + leaf_type.name + '"))) {\n'
            output += '       throw new InternalCompilerErrorException(s"' + exception_message + '")\n'
            output += '    }\n\n'

            # Calculate or indices
            indices = map(str, ([0] * depth) + [field_counter])
            field_counter = field_counter + 1

            output += '    block.getelementptr("'+ field_name + 'Ptr")(\n'
            output += '      elementType=' + field_name + 'IrType,\n'
            output += '      basePointer=valueCell,\n'
            output += '      indices=List(' + ", ".join(indices) + ').map(IntegerConstant(IntegerType(32), _)),\n'
            output += '      inbounds=true\n'
            output += '    )\n'
            output += '  }\n'
            output += '\n'
        
        output += '  def ' + store_method_name  + '(block : IrBlockBuilder)(toStore : IrValue, valueCell : IrValue) : Unit'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'

            pointer_name = field_name + 'Pointer'
            output += '    val ' + pointer_name + ' = ' + pointer_method_name + '(block)(valueCell)\n'
            output += '    block.store(toStore, ' + pointer_name + ', tbaaIndex=Some(tbaaIndex))\n' 
            output += '  }\n'
            output += '\n'
        
        output += '  def ' + load_method_name  + '(block : IrBlockBuilder)(valueCell : IrValue) : IrValue'
        if declare_only:
            output += '\n'
        else:
            output += ' = {\n'

            pointer_name = field_name + 'Pointer'
            output += '    val ' + pointer_name + ' = ' + pointer_method_name + '(block)(valueCell)\n'
            output += '    block.load("' + field_name + '")(' + pointer_name + ', tbaaIndex=Some(tbaaIndex))\n' 
            output += '  }\n'

    return output

def _generate_unconditional_type_check():
    output  = '  def genTypeCheck(startBlock : IrBlockBuilder)(valueCell : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {\n'
    output += '    startBlock.uncondBranch(successBlock)\n'
    output += '  }\n'

    return output

def _generate_type_check(all_types, cell_type):
    base_type_object = type_name_to_clike_class(BASE_TYPE)

    output  = '  def genTypeCheck(startBlock : IrBlockBuilder)(valueCell : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {\n'
    output += '    val datumValue = ' + base_type_object + '.genPointerBitcast(startBlock)(valueCell)\n'
    output += '    val typeId = DatumCell.genLoadFromTypeId(startBlock)(datumValue)\n'

    switch_params = []

    # Switch on the type ID
    switch_params.append("typeId")

    # If the type ID isn't known then fail the branch
    switch_params.append("failBlock")

    for concrete_type in cell_type.concrete_types.values():
        switch_params.append("(" + str(concrete_type.type_id) + "L -> successBlock)")
        
    output += '    startBlock.switch(' + ", ".join(switch_params) + ')\n'
    output += '  }\n'

    return output

def _generate_cell_types(all_types):
    base_type = all_types[BASE_TYPE]

    output  = GENERATED_FILE_COMMENT
    
    output += "package llambda.celltype\n\n"

    output += "import llambda.codegen.llvmir._\n"
    output += "import llambda.InternalCompilerErrorException\n\n"
        
    output += 'sealed abstract class CellType {\n'
    output += '  val name : String\n'
    output += '  val irType : FirstClassType\n'
    output += '  val supertype : Option[CellType]\n'
    output += '  val directSubtypes : Set[CellType]\n'
    output += '  val isAbstract : Boolean\n' 
    output += '  val tbaaIndex : Int\n' 
    output += '\n'
    output += '  def genTypeCheck(startBlock : IrBlockBuilder)(valueCell : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget)\n'
    output += '\n'
    output += '  def isTypeOrSubtypeOf(otherType : CellType) : Boolean = {\n'
    output += '    if (otherType == this) {\n'
    output += '      return true\n'
    output += '    }\n'
    output += '\n'
    output += '    supertype map (_.isTypeOrSubtypeOf(otherType)) getOrElse false\n'
    output += '  }\n'
    output += '\n'
    output += '  def isTypeOrSupertypeOf(otherType : CellType) : Boolean = {\n'
    output += '    if (otherType == this) {\n'
    output += '      return true\n'
    output += '    }\n'
    output += '\n'
    output += '    directSubtypes exists (_.isTypeOrSupertypeOf(otherType))\n'
    output += '  }\n'
    output += '\n'
    output += '  def concreteTypes : Set[ConcreteCellType] = this match {\n'
    output += '    case concreteType : ConcreteCellType => Set(concreteType)\n'
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

    output += 'sealed abstract class ConcreteCellType extends CellType {\n'
    output += '  val typeId : Int\n'
    output += '}\n\n'

    for type_name, cell_type in all_types.items():
        object_name = type_name_to_clike_class(type_name)
        supertype_name = cell_type.inherits

        if cell_type.abstract:
            scala_superclass = 'CellType'
        else:
            scala_superclass = 'ConcreteCellType'

        output += 'object ' + object_name + ' extends ' + scala_superclass + ' {\n'
        output += '  val name = "' + type_name + '"\n'
        output += '  val irType = UserDefinedType("' + type_name + '")\n'

        if supertype_name:
            supertype_object = type_name_to_clike_class(supertype_name)
            output += '  val supertype = Some(' + supertype_object + ')\n'
        else:
            output += '  val supertype = None\n'

        subtype_scala_names = []
        for subtype_name in cell_type.subtypes.keys():
            subtype_object = type_name_to_clike_class(subtype_name)
            subtype_scala_names.append(subtype_object)

        output += '  val directSubtypes = Set[CellType](' + ", ".join(subtype_scala_names) + ')\n'

        if cell_type.abstract:
            output += '  val isAbstract = true\n'
        else:
            output += '  val isAbstract = false\n'

        output += '  val tbaaIndex = ' + str(cell_type.index) + '\n'

        type_id = cell_type.type_id
        if type_id is not None:
            output += '  val typeId = ' + str(type_id) + '\n'
        output += "\n"
        
        output += _generate_field_types(cell_type)

        if not cell_type.singleton:
            output += '\n'
            output += _generate_constant_constructor(all_types, cell_type)
        
        output += '\n'

        if type_name == BASE_TYPE:
            # Every type should be an instance of the base type; generate a 
            # stub type check that always passes
            output += _generate_unconditional_type_check()
        else:
            output += _generate_type_check(all_types, cell_type)
        
        output += _generate_field_accessors(cell_type, cell_type)

        output += '}\n\n'

    return output

def _generate_name_to_cell_type(all_types):
    output  = GENERATED_FILE_COMMENT
    output += 'package llambda.frontend\n\n'

    output += 'import llambda.{celltype => ct}\n\n'

    output += 'object IntrinsicCellTypes {\n'
    output += '  def apply() : Map[String, ct.CellType] = Map(\n'

    map_members = []
    for type_name, cell_type in all_types.items():
        if cell_type.internal:
            # This isn't meant to be exposed as an NFI type
            continue

        nfi_decl_name = _type_name_to_nfi_decl(type_name)
        cell_type_class = type_name_to_clike_class(type_name)

        map_members.append('    ("<' + nfi_decl_name + '>" -> ' + 'ct.' + cell_type_class + ')')

    output += ',\n'.join(map_members) + '\n'

    output += '  )\n'
    output += '}\n'
    return output

def generate_scala_objects(all_types):
    ROOT_PATH = 'compiler/src/main/scala/llambda/'

    return {ROOT_PATH + 'celltype/generated/CellType.scala': _generate_cell_types(all_types),
            ROOT_PATH + 'frontend/generated/IntrinsicCellTypes.scala': _generate_name_to_cell_type(all_types)}
