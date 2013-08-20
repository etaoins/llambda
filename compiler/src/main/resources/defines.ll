; These are defined in core/constbools.cpp in our runtime
; The availabe_externally is just telling LLVM what they contain for optimization purposees
@lliby_true_value = available_externally constant %boolean {%boxedDatum {i16 4, i16 0}, i8 1}
@lliby_false_value = available_externally constant %boolean {%boxedDatum {i16 4, i16 0}, i8 0}
