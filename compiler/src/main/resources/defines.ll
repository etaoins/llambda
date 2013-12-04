; These are defined in core/constinstances.cpp in our runtime
@lliby_unspecific_value = external constant %unspecific
@lliby_false_value = external constant %boolean
@lliby_true_value = external constant %boolean
@lliby_empty_list_value = external constant %emptyList

; These are defined in alloc/allocator.cpp in our runtime
; They define the interface to our memory allocator
%cell = type {%pair}

@_lliby_alloc_start = external global %cell*
@_lliby_alloc_end = external global %cell*

; LLVM intrinsics
declare i1 @llvm.expect.i1(i1, i1)
