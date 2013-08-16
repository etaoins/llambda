from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE

def validate_boxed_types(boxed_types):
    # Ensure that our special types exist
    # These are types that we assume exist under the listed name for the
    # purposes code generation 
    SPECIAL_TYPES = [BASE_TYPE, "closure"]

    for special_type in SPECIAL_TYPES:
        if special_type not in boxed_types:
            raise SemanticException('Boxed type "' + special_type + '" is required to exist for code generation.')

    # Ensure type IDs are unique
    seen_type_ids = set()

    for name, boxed_type in boxed_types.items():
        type_id = boxed_type.type_id

        if type_id is None:
            continue

        if type_id in seen_type_ids:
            raise SemanticException('Boxed type "' + name + '" uses duplicate type ID ' + unicode(type_id))

        seen_type_ids.add(type_id)
    
    # Ensure that types inherits valid types
    for name, boxed_type in boxed_types.items():
        inherits = boxed_type.inherits

        if inherits is None:
            continue

        if inherits not in boxed_types:
            raise SemanticException('Boxed type "' + name + '" inherits from unknown boxed type "' + inherits + '"')
