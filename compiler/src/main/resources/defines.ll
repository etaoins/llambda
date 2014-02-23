; These are defined in core/constinstances.cpp in our runtime
@lliby_unit_value = external constant %unit
@lliby_false_value = external constant %boolean
@lliby_true_value = external constant %boolean
@lliby_empty_list_value = external constant %emptyList

%cell = type {%pair}

; LLVM intrinsics
declare i1 @llvm.expect.i1(i1, i1)

!0 = metadata !{ metadata !"World::allocNext" }
!1 = metadata !{ metadata !"World::allocEnd" }
; {allocNext, allocEnd}
%world = type {%cell*, %cell*}
