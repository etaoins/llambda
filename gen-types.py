#!/usr/bin/env python

import json
import codecs
from collections import OrderedDict

from typegen.celltype import *
from typegen.processtypetree import process_type_tree
from typegen.genllvmprelude import generate_llvm_prelude
from typegen.gencxxbinding import generate_cxx_binding
from typegen.genpredicates import generate_predicates
from typegen.genscalaobjects import generate_scala_objects
from typegen.gensizecheck import generate_size_check
from typegen.exceptions import SemanticException

with open('cellTypes.json') as f:
    cell_types_json = json.load(f)

cell_types = OrderedDict()

for type_json in cell_types_json:
    parsed_type = CellType(type_json)
    
    if parsed_type.name in cell_types:
        raise SemanticException('Duplicate type name "' + parsed_type.name + '"')

    cell_types[parsed_type.name] = parsed_type

process_type_tree(cell_types)

# Use our generator backends to build a dictionary of filename => content
output_files = {}
output_files.update(generate_llvm_prelude(cell_types))
output_files.update(generate_cxx_binding(cell_types))
output_files.update(generate_predicates(cell_types))
output_files.update(generate_scala_objects(cell_types))
output_files.update(generate_size_check(cell_types))

for filename, content in output_files.items():
    with codecs.open(filename, 'w', encoding='utf-8') as f:
        f.write(content)
