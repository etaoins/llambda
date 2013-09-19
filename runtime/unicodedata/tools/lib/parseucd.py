def parse_code_point(field):
    if field == "":
        return None

    return int(field, 16)

def parse_code_range(field):
    range_parts = field.split('..')
    range_part_count = len(range_parts)

    if range_part_count == 1:
        # Single code point. Make a single point range
        return (int(range_parts[0], 16), int(range_parts[0], 16))
    elif range_part_count == 2:
        # Proper range
        return (int(range_parts[0], 16), int(range_parts[1], 16))
    else:
        raise Exception("Unable to parse code range: " + field)

def parse_ucd_line(line):
    # Get rid of any comment
    data_line = line.split('#')[0].strip()

    if data_line == '':
        return None

    # Split on the field separator and strip space
    return [field.strip() for field in data_line.split(';')]
