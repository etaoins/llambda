; These are defined in core/constinstances.cpp in our runtime
@lliby_unspecific_value = external constant %unspecific
@lliby_false_value = external constant %boolean
@lliby_true_value = external constant %boolean
@lliby_empty_list_value = external constant %emptyList

; These are defined in alloc/allocator.cpp in our runtime
; They define the interface to our memory allocator
%cons = type {%pair}

@_lliby_alloc_start = external global %cons*
@_lliby_alloc_end = external global %cons*
