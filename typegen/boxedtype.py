import re
from collections import OrderedDict

from typegen.exceptions import SemanticException

class BoxedTypeField(object):
    def __init__(self, type_name, field_json):
        self.name = field_json['name']

        self.llvm_type    = field_json.get('llvmType', None)
        self.complex_type = field_json.get('complexType', None)
        self.signed       = field_json.get('signed', None)

        self.qualified_name = type_name + "." + self.name

        # Integers must specify their signedness
        if self.llvm_type and re.match("^i\\d+$", self.llvm_type) and (self.signed is None):
            raise SemanticException('Integer field "' + self.qualified_name + '" does not specify signedness')

        if self.llvm_type is None and self.complex_type is None:
            raise SemanticException('Field "' + self.qualified_name + '" does not have a type')
        
        if self.llvm_type is not None and self.complex_type is not None:
            raise SemanticException('Field "' + self.qualified_name + '" has both a simple and complex type defined')

class BoxedType(object):
    def __init__(self, type_json):
        self.name     = type_json['name']
        self.abstract = type_json.get('abstract', False)
        self.type_id  = type_json.get('typeId', None)
        self.inherits = type_json['inherits']

        type_id = self.type_id

        if not isinstance(type_id, int): 
            if not self.abstract:
                raise SemanticException('Type "' + self.name + '" does not have a specific type ID and must be marked abstract')
            elif type_id is not None:
                # Besides integers we only understand bit test type IDs (eg &32768)
                if type_id[0] != '&':
                    raise SemanticException('Unable to parse type ID "' + type_id + '"')

        self.fields = OrderedDict()
        
        for field_json in type_json['fields']:
            parsed_field = BoxedTypeField(self.name, field_json)
            
            if parsed_field.name in self.fields:
                raise SemanticException('Duplicate field name "' + parsed_field.qualified_name + '"')

            self.fields[parsed_field.name] = parsed_field
