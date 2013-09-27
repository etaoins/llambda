from typegen.exceptions import SemanticException
from typegen.constants import BASE_TYPE

def process_type_tree(boxed_types):
    # Ensure that our special types exist
    # These are types that we assume exist under the listed name for the
    # purposes code generation 
    SPECIAL_TYPES = [BASE_TYPE, "closure"]

    for special_type in SPECIAL_TYPES:
        if special_type not in boxed_types:
            raise SemanticException('Boxed type "' + special_type + '" is required to exist for code generation.')

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

    # Ensure type assertions are unique
    seen_type_assertions = set()

    for name, boxed_type in boxed_types.items():
        type_assertion = boxed_type.type_assertion

        if type_assertion is None:
            continue

        if type_assertion in seen_type_assertions:
            raise SemanticException('Boxed type "' + name + '" uses duplicate type assertion')

        seen_type_assertions.add(type_assertion)
    
