from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE

def process_type_tree(cell_types):
    # Ensure that our special types exist
    # These are types that we assume exist under the listed name for the
    # purposes code generation 
    SPECIAL_TYPES = [BASE_TYPE, "procedure", "listElement"]

    for special_type in SPECIAL_TYPES:
        if special_type not in cell_types:
            raise SemanticException('Cell type "' + special_type + '" is required to exist for code generation.')
    
    # Assign each type an index. This is used for TBAA
    type_index = 0

    for cell_type in cell_types.values():
        cell_type.index = type_index
        type_index = type_index + 1

    # Ensure that types inherit valid types and add them as subtypes where requires
    for name, cell_type in cell_types.items():
        inherits = cell_type.inherits

        if inherits is None:
            continue

        try:
            supertype = cell_types[inherits]
        except KeyError:
            raise SemanticException('Cell type "' + name + '" inherits from unknown type "' + inherits + '"')

        # Concrete types are also final
        if supertype.concrete:
            raise SemanticException('Cell type "' + name + '" inherits from concrete type "' + inherits + '"') 

        cell_type.supertype = supertype
        supertype.add_subtype(cell_type)
    
    # Ensure that it's possible to have instances of each type
    for name, cell_type in cell_types.items():
        if not cell_type.concrete_types:
            raise SemanticException('Cell type "' + name + '" cannot have instances. It must either have subtypes or be concrete')

    # Ensure type IDs are unique
    seen_type_ids = set()

    for name, cell_type in cell_types.items():
        type_id = cell_type.type_id

        if type_id is None:
            continue

        if type_id in seen_type_ids:
            raise SemanticException('Cell type "' + name + '" uses duplicate type ID')

        seen_type_ids.add(type_id)
