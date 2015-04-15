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
%world = type {%shadowStackEntry*, %cell*, %cell*}

!3 = !{ !"ShadowStackEntry::next" }
!4 = !{ !"ShadowStackEntry::cellCount" }
!5 = !{ !"ShadowStackEntry::roots" }

!6 = !{ !"VectorCell::m_elements" }

; {next, cellCount, roots}
%shadowStackEntry = type {%shadowStackEntry*, i64, [0 x %any*]}

; {refcount, data}
%sharedByteArray = type {i32, [0 x i8]}
