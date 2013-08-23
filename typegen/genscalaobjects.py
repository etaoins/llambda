from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE
from typegen.clikeutil import *

def generate_scala_objects(boxed_types):
    output  = GENERATED_FILE_COMMENT
    
    output += "package llambda.codegen.boxedtype\n\n"

    output += "import llambda.codegen.llvmir.{UserDefinedType, PointerType}\n\n"
        
    output += 'sealed abstract class BoxedType {\n'
    output += '  val irType : PointerType\n'
    output += '}\n\n'

    for type_name, boxed_type in boxed_types.items():
        object_name = type_name_to_clike_class(type_name)

        output += 'object ' + object_name + ' extends BoxedType {\n'
        output += '  val irType = PointerType(UserDefinedType("' + type_name + '"))\n'
        output += '}\n\n'

    return {'compiler/src/main/scala/llambda/codegen/generated/BoxedType.scala': output}
