; These are defined in core/constinstances.cpp in our runtime
@llcore_unit_value = external constant %unit
@llcore_false_value = external constant %boolean
@llcore_true_value = external constant %boolean
@llcore_empty_list_value = external constant %emptyList

%cell = type {[32 x i8]}

!0 = !{ !"ROOT" }
!1 = !{ !"World::shadowStackHead", !0 }
!2 = !{ !"World::allocNext", !0 }
!3 = !{ !"World::allocEnd", !0 }

; {shadowStackHead, allocNext, allocEnd}
%world = type {%shadowStackEntryHeader*, %cell*, %cell*}

!4 = !{ !"ShadowStackEntryHeader::next", !0 }
!5 = !{ !"ShadowStackEntryHeader::cellCount", !0 }

!6 = !{ !"VectorCell::m_elements", !0 }

; {next, cellCount}
%shadowStackEntryHeader = type {%shadowStackEntryHeader*, i32}

; {refcount, data}
%sharedByteArray = type {i32, i32, [0 x i8]}
