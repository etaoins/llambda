class SharedHashChains:
    def __init__(self):
        self.next_chain_index = 0
        self.index_for_chain = dict()
        self.source_lines = []

    def has_chain_entry(self, chain_values):
        chain_key = str(chain_values) 
        return chain_key in self.index_for_chain

    def add_unique_chain_entry(self, chain_values):
        # Does this chain already exists?
        chain_key = str(chain_values) 

        try:
            return self.index_for_chain[chain_key]
        except KeyError:
            pass

        chain_entries = []
        for (index, (key, value)) in enumerate(chain_values):
            if index == len(chain_values) - 1:
                last_value = "1"
            else:
                last_value = "0"

            # Build this C++ chain entry
            chain_entry = "{" + last_value + ", " + hex(key) + ", " + hex(value) + "}"
            chain_entries.append(chain_entry)

        # Add this line to our source code
        self.source_lines.append("\t" + ", ".join(chain_entries) + ",\n")
        
        # Move the next chain index forward
        new_chain_index = self.next_chain_index
        self.next_chain_index += len(chain_values)

        # Record this chain key so we can share this hash chain if we see it
        # later
        self.index_for_chain[chain_key] = new_chain_index

        return new_chain_index

    def gen(self):
        output  = "const NonAsciiHashChain NonAsciiHashChains[] = {\n"
        output += "".join(self.source_lines)
        output += "};\n\n"

        return output

def _hash_function(code_point, nonascii_hash_size):
    return ((code_point * 2654435761) & 0xFFFFFFFF) % nonascii_hash_size

def _cost_for_candidate_hash(candidate_hash, shared_chains):
    # The bucket itself costs 1 if it's filled, empty or spilt
    hash_cost = len(candidate_hash)

    for bucket_index in range(len(candidate_hash)):
        # Every spilt value costs 1
        chain_values = candidate_hash[bucket_index]
        entry_count = len(chain_values)

        if entry_count > 1 and not shared_chains.has_chain_entry(chain_values):
            hash_cost = hash_cost + entry_count
    return hash_cost

def _find_optimal_hash(input_dict, shared_chains):
    best_cost = None
    best_hash = None
    input_size = len(input_dict)

    for candidate_hash_size in range(input_size, int(input_size * 1.5) + 2):
        # Build a candidate hash
        candidate_hash = [[] for _ in range(candidate_hash_size)] 
    
        for (code_point, value) in input_dict.items():
            if code_point >= 128:
                hash_index = _hash_function(code_point, candidate_hash_size)
                candidate_hash[hash_index].append((code_point, value))

        cost = _cost_for_candidate_hash(candidate_hash, shared_chains)

        if best_cost is None or (cost <= best_cost):
            best_cost = cost
            best_hash = candidate_hash

    return best_hash

def gen_hashtable(base_name, input_dict, shared_chains):
    nonascii_hash = _find_optimal_hash(input_dict, shared_chains)
    nonascii_hash_size = len(nonascii_hash)

    ascii_table_name = base_name + "AsciiTable"
    nonascii_hash_name = base_name + "NonAsciiHash"

    # The ASCII table is direct mapped
    ascii_table = [None for _ in range(128)]
    for (code_point, value) in input_dict.items():
        if code_point < 128:
            ascii_table[code_point] = value

    output = ""
    
    # Build the direct mapped ASCII table C++
    output += "const AsciiTableEntry " + ascii_table_name + "[128] = {\n"
    for code_point in range(128):
        value = ascii_table[code_point]

        if value is None:
            output += "\t   -1,"
        else:
            output += "\t" + hex(value).rjust(5) + ","

        if ((code_point + 1) % 8) == 0:
            output += "\n"

    output += "};\n\n"
 
    # Build the hash buckets
    output += "const NonAsciiHashBucket " + nonascii_hash_name + "[" + str(nonascii_hash_size) + "] = {\n"
    for bucket_index in range(nonascii_hash_size):
        chain_values = nonascii_hash[bucket_index]

        if (len(chain_values) == 0):
            output += "\t{.chain = nullptr},\n"
        elif (len(chain_values) == 1):
            (only_key, only_value) = chain_values[0]

            output += "\t{.codePoint = " + hex(only_key) + ", .value = " + hex(only_value) + ", .isInline = 1},\n"

        else:
            chain_index = shared_chains.add_unique_chain_entry(chain_values)
            output += "\t{.chain = &NonAsciiHashChains[" + str(chain_index) + "]},\n"
    output += "};\n\n"

    output += "const UnicodeHash " + base_name + "Hash = {\n"
    output += "\t.asciiTable = &" + ascii_table_name + "[0],\n" 
    output += "\t.nonAsciiHash = &" + nonascii_hash_name + "[0],\n" 
    output += "\t.nonAsciiHashSize = sizeof(" + nonascii_hash_name + ") / sizeof(NonAsciiHashBucket)\n"
    output += "};\n\n"

    return output
