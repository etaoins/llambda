#!/usr/bin/env python

import sys
from os import path
from lib.parseucd import *
from lib.genhashtable import gen_hashtable_cpp
from lib.genrangelist import gen_rangelist_cpp

if len(sys.argv) != 2:
    sys.stderr.write("Usage: " + sys.argv[0] + " ucd_directory\n")
    sys.exit(1)

def set_dict_range(target_dict, index_range, value):
    for index in range(index_range[0], index_range[1] + 1):
        target_dict[index] = value

ucd_directory = sys.argv[1]

lowercase_ranges = []
uppercase_ranges = []
alphabetic_ranges = []
whitespace_ranges = []

toupper_dict = {}
tolower_dict = {}
tofolded_dict = {}
numeric_value_dict = {}

# Parse UnicodeData.txt first
with open(path.join(ucd_directory, 'UnicodeData.txt'), 'r') as f:
    for line in f:
        fields = parse_ucd_line(line)

        # UnicodeData.txt doesn't support proper ranges for historic reasons
        start_code_point = parse_code_point(fields[0])

        name = fields[1]
        general_category = fields[2]
        numeric_value_string = fields[6]
        uppercase_mapping = parse_code_point(fields[12])
        lowercase_mapping = parse_code_point(fields[13])

        if (name.endswith(", First>")):
            # This is a range
            next_line = f.__next__()
            end_code_point = parse_code_point(parse_ucd_line(next_line)[0])
        else:
            end_code_point = start_code_point

        code_range = (start_code_point, end_code_point)

        if general_category == 'Lu':
            uppercase_ranges.append(code_range)
        elif general_category == 'Ll':
            lowercase_ranges.append(code_range)

        if general_category in ["Lu", "Ll", "Lt", "Lm", "Lo", "Nl"]:
            alphabetic_ranges.append(code_range)

        if lowercase_mapping is not None:
            set_dict_range(tolower_dict, code_range, lowercase_mapping)
        
        if uppercase_mapping is not None:
            set_dict_range(toupper_dict, code_range, uppercase_mapping)

        if numeric_value_string != "":
            numeric_value = int(numeric_value_string, 10)
            set_dict_range(numeric_value_dict, code_range, numeric_value)

# Now PropList.txt
with open(path.join(ucd_directory, 'PropList.txt'), 'r') as f:
    for line in f:
        fields = parse_ucd_line(line)

        if fields is None:
            # Empty line or comment
            continue

        code_range = parse_code_range(fields[0])
        property_name = fields[1]

        if property_name == 'White_Space':
            whitespace_ranges.append(code_range)
        elif property_name == 'Other_Alphabetic':
            alphabetic_ranges.append(code_range)
        elif property_name == 'Other_Uppercase':
            uppercase_ranges.append(code_range)
        elif property_name == 'Other_Lowercase':
            lowercase_ranges.append(code_range)

# Finally CaseFolding.txt
with open(path.join(ucd_directory, 'CaseFolding.txt'), 'r') as f:
    for line in f:
        fields = parse_ucd_line(line)

        if fields is None:
            # Empty line or comment
            continue

        code_range = parse_code_range(fields[0])
        status = fields[1]

        # R7RS requires that simple folding is used
        if (status == 'C') or (status == 'S'):
            folded_code_point = parse_code_point(fields[2])
            set_dict_range(tofolded_dict, code_range, folded_code_point)

print(gen_rangelist_cpp("Uppercase", uppercase_ranges))
print(gen_rangelist_cpp("Lowercase", lowercase_ranges))
print(gen_rangelist_cpp("Alphabetic", alphabetic_ranges))
print(gen_rangelist_cpp("Whitespace", whitespace_ranges))

print(gen_hashtable_cpp("ToUpper", toupper_dict))
print(gen_hashtable_cpp("ToLower", tolower_dict))
print(gen_hashtable_cpp("ToFolded", tofolded_dict))
print(gen_hashtable_cpp("ToNumericValue", numeric_value_dict))
