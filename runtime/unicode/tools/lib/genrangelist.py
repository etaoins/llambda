def _merge_rangelist(unsorted_ranges):
    if len(unsorted_ranges) == 0:
        return []

    # First sort the ranges
    sorted_ranges = sorted(unsorted_ranges)

    merged_ranges = []

    # Start our state
    (merged_start, merged_end) = sorted_ranges.pop(0)

    for (range_start, range_end) in sorted_ranges:
        if range_start <= (merged_end + 1):
            # Merge this with the existing range
            merged_end = range_end
        else:
            # We have a full range - append it
            merged_ranges.append((merged_start, merged_end))

            # Start a new merged range
            merged_start = range_start
            merged_end = range_end

    # Append the last ranged
    merged_ranges.append((merged_start, merged_end))

    return merged_ranges

def gen_rangelist_cpp(base_name, rangelist):
    rangelist = _merge_rangelist(rangelist)
   
    output  = ""
    output += "const UnicodeRange " + base_name + "Ranges[] = {\n"

    entry_index = None

    for (index, (range_start, range_end)) in enumerate(rangelist):
        # Split the top of the binary tree in to ASCII and non-ASCII halves
        # This is to speed to ASCII searches
        if range_end <= 127:
            entry_index = index

        output += "\t{" + hex(range_start) + ", " + hex(range_end) + "},\n"
    
    output += "};\n\n"

    output += "const UnicodeRangeSet " + base_name + "RangeSet = {\n"
    output += "\t.ranges = &" + base_name + "Ranges[0],\n"
    output += "\t.rangeCount = " + str(len(rangelist)) + ",\n"
    output += "\t.entryRange = " + str(entry_index) + "\n" 
    output += "};\n\n"
    
    return output
