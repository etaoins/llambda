import re
from collections import OrderedDict

from typegen.exceptions import SemanticException

class TypeCondition(object):
    pass

class TypeEqualsCondition(TypeCondition):
    def __init__(self, boxed_type, type_id):
        self.boxed_type = boxed_type
        self.type_id = type_id

class TypeBitmaskCondition(TypeCondition):
    def __init__(self, boxed_type, bitmask):
        self.boxed_type = boxed_type
        self.bitmask = bitmask

class BoxedTypeField(object):
    def __init__(self, type_name, field_json):
        self.name = field_json['name']

        self.llvm_type    = field_json.get('llvmType', None)
        self.complex_type = field_json.get('complexType', None)
        self.signed       = field_json.get('signed', None)

        self.qualified_name = type_name + "." + self.name

        # Integers must specify their signedness
        if self.llvm_type and re.match("^i\\d+$", self.llvm_type) and not isinstance(self.signed, bool):
            raise SemanticException('Integer field "' + self.qualified_name + '" does not specify signedness')

        if self.llvm_type is None and self.complex_type is None:
            raise SemanticException('Field "' + self.qualified_name + '" does not have a type')
        
        if self.llvm_type is not None and self.complex_type is not None:
            raise SemanticException('Field "' + self.qualified_name + '" has both a simple and complex type defined')

class BoxedType(object):
    def __init__(self, type_json):
        self.name      = type_json['name']
        self.abstract  = type_json.get('abstract', False)
        self.singleton = type_json.get('singleton', False)
        
        raw_type_id = type_json.get('typeId', None)
        
        self.inherits = type_json['inherits']

        # These are populated in processtypetree
        self.supertype = None
        self.subtypes = OrderedDict()
        self.index = None

        if isinstance(raw_type_id, int): 
            self.type_id = raw_type_id
            self._type_conditions = [TypeEqualsCondition(self, raw_type_id)]
        else:
            self.type_id = None

            if not self.abstract:
                raise SemanticException('Type "' + self.name + '" does not have a specific type ID and must be marked abstract')
            
            if raw_type_id is None:
                self._type_conditions = None
            else:
                # Besides integers we only understand bit test type IDs (eg &0x8000)
                if raw_type_id[0] != '&':
                    raise SemanticException('Unable to parse type ID "' + raw_type_id + '"')
                else:
                    self._type_conditions = [TypeBitmaskCondition(self, int(raw_type_id[1:], 0))]

        self.fields = OrderedDict()
        
        for field_json in type_json['fields']:
            parsed_field = BoxedTypeField(self.name, field_json)
            
            if parsed_field.name in self.fields:
                raise SemanticException('Duplicate field name "' + parsed_field.qualified_name + '"')

            self.fields[parsed_field.name] = parsed_field

    @property
    def type_conditions(self):
        # Do we have a condition from either an explicitly defined condition or 
        # one calculated on a previous invocation?
        if self._type_conditions is None:
            self._type_conditions = []

            # Collect all the conditions for our subtypes
            for subtype in self.subtypes.values():
                self._type_conditions.extend(subtype.type_conditions)

        return self._type_conditions

    def add_subtype(self, subtype):
        self.subtypes[subtype.name] = subtype
