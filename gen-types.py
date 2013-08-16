#!/usr/bin/env python

import json
import codecs
from collections import OrderedDict

from typegen.boxedtype import *
from typegen.validate import validate_boxed_types
from typegen.genllvmprelude import generate_llvm_prelude
from typegen.gencxxbinding import generate_cxx_binding
from typegen.exceptions import SemanticException

with open('boxedTypes.json') as f:
    boxed_types_json = json.load(f)

boxed_types = OrderedDict()

for type_json in boxed_types_json:
    parsed_type = BoxedType(type_json)
    
    if parsed_type.name in boxed_types:
        raise SemanticException('Duplicate type name "' + parsed_type.name + '"')

    boxed_types[parsed_type.name] = parsed_type

validate_boxed_types(boxed_types)

# Use our generator backends to build a dictionary of filename => content
output_files = {}
output_files.update(generate_llvm_prelude(boxed_types))
output_files.update(generate_cxx_binding(boxed_types))

for filename, content in output_files.items():
    with codecs.open(filename, 'w', encoding='utf-8') as f:
        f.write(content)
