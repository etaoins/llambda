import re

from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def _uppercase_first(string):
    return string[0].upper() + string[1:]

def _type_name_to_nfi_decl(type_name):
    return "boxed-" + re.sub('([A-Z][a-z]+)', r'-\1', type_name).lower()

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

def _generate_constant_constructor(all_types, boxed_type):
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
        output += '      throw new InternalCompilerErrorException("' + exception_message + '")\n'
        output += '    }\n\n'

    output += '    StructureConstant(List(\n'

    supertype = boxed_type.supertype
    if supertype:
        output += '      supertype.get.createConstant(\n'

        super_parameters = []

        for field_name in recursive_field_names:
            if (field_name == 'typeId') and (constant_type_id is not None):
                type_id_field = all_types[BASE_TYPE].fields['typeId']
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
            gcstate_field = all_types[BASE_TYPE].fields['gcState']
            gcstate_type = _field_type_to_scala(gcstate_field)
            
            our_fields.append('      IntegerConstant(' + gcstate_type + ', 0)')
        else:
            our_fields.append('      ' + field_name)

    output += ",\n".join(our_fields) + "\n"

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
            output += '      elementType=' + _field_type_to_scala(field) + ',\n'
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
    type_id_type = _field_type_to_scala(all_types[BASE_TYPE].fields['typeId'])

    output  = '  def genTypeCheck(startBlock : IrBlockBuilder)(boxedValue : IrValue, successBlock : IrBranchTarget, failBlock : IrBranchTarget) {\n'
    output += '    val datumValue = ' + base_type_object + '.genPointerBitcast(startBlock)(boxedValue)\n'
    output += '    val typeIdPointer = ' + base_type_object + '.genPointerToTypeId(startBlock)(datumValue)\n'
    output += '    val typeId = startBlock.load("typeId")(typeIdPointer)\n'

    # Start building off the start block
    incoming_block = 'start'

    for idx, concrete_type in enumerate(boxed_type.concrete_types.values()):
        checking_type_name = concrete_type.name
        uppercase_type_name = _uppercase_first(checking_type_name)
        
        check_bool_name = 'is' + uppercase_type_name
        expected_value = 'IntegerConstant(' + type_id_type + ', ' + str(concrete_type.type_id) + ')'

        output += '\n' 
        output += '    val ' + check_bool_name + ' = '  + incoming_block + 'Block.icmp("' + check_bool_name + '")(ComparisonCond.Equal, None, typeId, ' + expected_value + ')\n'
            
        if idx == (len(boxed_type.concrete_types) - 1):
            # No more conditions; exit using the fail block if this check fails
            outgoing_block = 'fail'
        else:
            # Allocate the outgoing block so we can use it as a brach target
            outgoing_block = 'not' + uppercase_type_name
            output += '    val ' + outgoing_block + 'Block = ' + incoming_block + 'Block.startChildBlock("' + outgoing_block + '")\n'

        output += '    ' + incoming_block + 'Block.condBranch(' + check_bool_name + ', successBlock, ' + outgoing_block + 'Block)\n'

        # The block we just branched to is the block we're building next
        incoming_block = outgoing_block

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

    output += 'object NativeTypeNameToBoxedType {\n'
    output += '  def apply : PartialFunction[String, bt.BoxedType] = {\n'

    for type_name, boxed_type in all_types.items():
        nfi_decl_name = _type_name_to_nfi_decl(type_name)
        boxed_type_class = type_name_to_clike_class(type_name)

        output += '    case "' + nfi_decl_name + '" => '
        output += 'bt.' + boxed_type_class + '\n'

    output += '  }\n'
    output += '}\n'
    return output

def generate_scala_objects(all_types):
    ROOT_PATH = 'compiler/src/main/scala/llambda/'

    return {ROOT_PATH + 'boxedtype/generated/BoxedType.scala': _generate_boxed_types(all_types),
            ROOT_PATH + 'frontend/generated/NativeTypeNameToBoxedType.scala': _generate_name_to_boxed_type(all_types)}
