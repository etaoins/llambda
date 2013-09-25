import re
from collections import OrderedDict

from typegen.exceptions import SemanticException

class TypeAssertion(object):
    pass

class TypeEqualsAssertion(TypeAssertion):
    def __init__(self, boxed_type, type_id_value):
        self.boxed_type = boxed_type
        self.type_id_value = type_id_value

class TypeBitmaskAssertion(TypeAssertion):
    def __init__(self, boxed_type, bitmask):
        self.boxed_type = boxed_type
        self.bitmask = bitmask

class TypeUnionAssertion(TypeAssertion):
    def __init__(self, subassertions):
        self.subassertions = subassertions

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
        raw_type_id  = type_json.get('typeId', None)
        self.inherits = type_json['inherits']
        self.subtypes = OrderedDict()

        if isinstance(raw_type_id, int): 
            self._type_assertion = TypeEqualsAssertion(self, raw_type_id)
        else:
            if not self.abstract:
                raise SemanticException('Type "' + self.name + '" does not have a specific type ID and must be marked abstract')
            elif raw_type_id is None:
                self._type_assertion = None
            else:
                # Besides integers we only understand bit test type IDs (eg &32768)
                if raw_type_id[0] != '&':
                    raise SemanticException('Unable to parse type ID "' + raw_type_id + '"')
                else:
                    self._type_assertion = TypeBitmaskAssertion(self, raw_type_id[1:]) 

        self.fields = OrderedDict()
        
        for field_json in type_json['fields']:
            parsed_field = BoxedTypeField(self.name, field_json)
            
            if parsed_field.name in self.fields:
                raise SemanticException('Duplicate field name "' + parsed_field.qualified_name + '"')

            self.fields[parsed_field.name] = parsed_field
    
    @property
    def type_assertion(self):
        # Do we have a assertion from either an explicitly defined assertion or 
        # one calculated on a previous invocation?
        if self._type_assertion is None:
            subtype_assertions = []

            # Collect all the assertions for our subtypes
            for subtype in self.subtypes.values():
                subtype_assertions.append(subtype.type_assertion)

            if len(subtype_assertions) != 0:
                # Wrap the subtype assertions in a union
                self._type_assertion = TypeUnionAssertion(subtype_assertions)

        return self._type_assertion

    def add_subtype(self, subtype):
        self.subtypes[subtype.name] = subtype
