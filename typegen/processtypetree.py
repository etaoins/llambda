from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE

def process_type_tree(boxed_types):
    # Ensure that our special types exist
    # These are types that we assume exist under the listed name for the
    # purposes code generation 
    SPECIAL_TYPES = [BASE_TYPE, "procedure", "listElement"]

    for special_type in SPECIAL_TYPES:
        if special_type not in boxed_types:
            raise SemanticException('Boxed type "' + special_type + '" is required to exist for code generation.')
    
    # Assign each type an index. This is used for TBAA
    type_index = 0

    for boxed_type in boxed_types.values():
        boxed_type.index = type_index
        type_index = type_index + 1

    # Ensure that types inherit valid types and add them as subtypes where requires
    for name, boxed_type in boxed_types.items():
        inherits = boxed_type.inherits

        if inherits is None:
            continue

        try:
            supertype = boxed_types[inherits]
        except KeyError:
            raise SemanticException('Boxed type "' + name + '" inherits from unknown boxed type "' + inherits + '"')

        if supertype.singleton:
            raise SemanticException('Boxed type "' + name + '" inherits from singleton type "' + inherits + '"') 

        boxed_type.supertype = supertype
        supertype.add_subtype(boxed_type)
    
    # Ensure that it's possible to have instances of each type
    for name, boxed_type in boxed_types.items():
        if not boxed_type.type_conditions:
            raise SemanticException('Boxed type "' + name + '" cannot have instances. It must either have subtypes or be concrete')

    # Ensure type IDs are unique
    seen_type_ids = set()

    for name, boxed_type in boxed_types.items():
        type_id = boxed_type.type_id

        if type_id is None:
            continue

        if type_id in seen_type_ids:
            raise SemanticException('Boxed type "' + name + '" uses duplicate type ID')

        seen_type_ids.add(type_id)
    
