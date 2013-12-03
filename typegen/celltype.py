import re
from collections import OrderedDict

from typegen.exceptions import SemanticException

class CellTypeField(object):
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

class CellType(object):
    def __init__(self, type_json):
        self.name      = type_json['name']
        self.singleton = type_json.get('singleton', False)
        
        self.type_id = type_json.get('typeId', None)
        self.inherits = type_json['inherits']
        self.internal = type_json.get('internal', False)
        
        # This is just to test that this agrees with the presence or absence
        # of "type_id"
        marked_abstract  = type_json.get('abstract', False)

        # These are populated in processtypetree
        self.supertype = None
        self.subtypes = OrderedDict()
        self.index = None

        # Types must either have a type ID or be abstract
        if (self.type_id is None) and (not marked_abstract):
            raise SemanticException('Type "' + self.name + '" does not have a type ID and must be marked abstract')
        elif (self.type_id is not None) and marked_abstract: 
            raise SemanticException('Type "' + self.name + '" has a type ID and cannot be marked abstract')

        # Singleton types must be concrete
        if self.singleton and self.abstract:
            raise SemanticException('Type "' + self.name + '" is a singleton and cannot be abstract')

        self.fields = OrderedDict()
        
        for field_json in type_json['fields']:
            parsed_field = CellTypeField(self.name, field_json)
            
            if parsed_field.name in self.fields:
                raise SemanticException('Duplicate field name "' + parsed_field.qualified_name + '"')

            self.fields[parsed_field.name] = parsed_field

    @property
    def abstract(self):
        return self.type_id is None
    
    @property
    def concrete(self):
        return self.type_id is not None

    @property
    def concrete_types(self):
        if self.type_id is not None:
            return OrderedDict({self.name: self}) 
        else:
            types = OrderedDict()

            for subtype in self.subtypes.values():
                types.update(subtype.concrete_types)

            return types

    def add_subtype(self, subtype):
        self.subtypes[subtype.name] = subtype
