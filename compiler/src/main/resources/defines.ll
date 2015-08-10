; These are defined in core/constinstances.cpp in our runtime
@llcore_unit_value = external constant %unit
@llcore_false_value = external constant %boolean
@llcore_true_value = external constant %boolean
@llcore_empty_list_value = external constant %emptyList

%cell = type {[24 x i8]}

!0 = !{ !"World::shadowStackHead" }
!1 = !{ !"World::allocNext" }
!2 = !{ !"World::allocEnd" }

; {shadowStackHead, allocNext, allocEnd}
%world = type {%shadowStackEntryHeader*, %cell*, %cell*}

!3 = !{ !"ShadowStackEntryHeader::next" }
!4 = !{ !"ShadowStackEntryHeader::cellCount" }

!5 = !{ !"VectorCell::m_elements" }

; {next, cellCount}
%shadowStackEntryHeader = type {%shadowStackEntryHeader*, i32}

; {refcount, data}
%sharedByteArray = type {i32, i32, [0 x i8]}
