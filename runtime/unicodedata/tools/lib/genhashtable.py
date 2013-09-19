def _primes_less_than(n):
    composite_array = bytearray(n)

    step = 2
    primes = []

    while True:
        # Mark all muliples composite
        for composite in range(step, n, step):
            composite_array[composite] = 1

        # Find the next step
        for candidate_prime in range(step, n):
            if composite_array[candidate_prime] == 0:
                primes.append(candidate_prime)
                step = candidate_prime
                break
        else:
            break

    return primes

_prime_cache = None

def _chain_name(hashtable_name, index):
    return hashtable_name + "HashChain" + str(index)

def _hash_function(code_point, nonascii_hash_size):
    return (code_point * 2654435761) % nonascii_hash_size

def gen_hashtable_cpp(base_name, input_dict):
    global _prime_cache

    # Build some prime numbers for our hash table size
    if _prime_cache is None:
        _prime_cache = _primes_less_than(100000)

    # Find the first prime > our input size
    # Note that ASCII goes in to its own table so we're underestimating the
    # size slightly. This should help reduce collisions a bit
    for nonascii_hash_size in _prime_cache:
        if nonascii_hash_size > len(input_dict):
            break
    else:
        raise Exception("Need more prime numbers!")

    ascii_table_name = base_name + "AsciiTable"
    nonascii_hash_name = base_name + "NonAsciiHash"

    # The ASCII table is direct mapped
    ascii_table = [None for _ in range(128)]
    nonascii_hash = [[] for _ in range(nonascii_hash_size)] 

    for (code_point, value) in input_dict.items():
        if code_point < 128:
            ascii_table[code_point] = value
        else:
            hash_index = _hash_function(code_point, nonascii_hash_size)
            nonascii_hash[hash_index].append((code_point, value))

    output = ""
    
    # Build the direct mapped ASCII table C++
    output += "const AsciiTableEntry " + ascii_table_name + "[128] = {\n"
    for code_point in range(128):
        value = ascii_table[code_point]

        if value is None:
            output += "\t-1,\n"
        else:
            output += "\t" + hex(value) + ",\n"
    output += "};\n\n"

    # Build the hash chains for any values that have spilt their buckets
    for hash_index in range(nonascii_hash_size):
        chain_values = nonascii_hash[hash_index]

        if (len(chain_values) < 2):
            # Nothing to do
            continue
        
        chain_name = _chain_name(base_name, hash_index)
        chain_size = len(chain_values)

        output += "const NonAsciiHashChain " + chain_name + "[" + str(chain_size) + "] = {\n"

        for (index, (key, value)) in enumerate(chain_values):
            if index == len(chain_values) - 1:
                last_value = "1"
            else:
                last_value = "0"

            output += "\t{" + last_value + ", " + hex(key) + ", " + hex(value) + "},\n" 

        output += "};\n\n"
 
    # Build the hash buckets
    output += "const NonAsciiHashBucket " + nonascii_hash_name + "[" + str(nonascii_hash_size) + "] = {\n"
    for hash_index in range(nonascii_hash_size):
        chain_values = nonascii_hash[hash_index]

        if (len(chain_values) == 0):
            output += "\t{.chain = nullptr},\n"
        elif (len(chain_values) == 1):
            (only_key, only_value) = chain_values[0]

            output += "\t{.isInline = 1, .codePoint = " + hex(only_key) + ", .value = " + hex(only_value) + "},\n"

        else:
            output += "\t{.chain = &" + _chain_name(base_name, hash_index) + "[0]},\n"
    output += "};\n\n"

    output += "const UnicodeHash " + base_name + "Hash = {\n"
    output += "\t.asciiTable = &" + ascii_table_name + "[0],\n" 
    output += "\t.nonAsciiHash = &" + nonascii_hash_name + "[0],\n" 
    output += "\t.nonAsciiHashSize = sizeof(" + nonascii_hash_name + ") / sizeof(NonAsciiHashBucket)\n"
    output += "};\n\n"

    return output
