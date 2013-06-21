; {type, gcstate}
%genericExpr = type { i16, i16 }

; {type, gcstate, length, data }
%stringExpr = type { i16, i16, i32, i8* }
; {type, gcstate, length, data}
%symbolExpr = type { i16, i16, i32, i8* }

; {type, gcstate, truthiness}
%booleanExpr = type { i16, i16, i32 }

; {type, gcstate, value}
%integerExpr = type { i16, i16, i64 }
; {type, gcstate, value}
%floatExpr = type { i16, i16, double }

; {type, gcstate, car, cdr]
%pairExpr = type { i16, i16, %genericExpr*, %genericExpr* }

; {type, gcstate, length, data}
%vectorLikeExpr = type { i16, i16, i32, %genericExpr** }

; {type, gcstate, code point}
%characterExpr = type { i16, i16, i32 }

; {type, gcstate, closure, (entry)(closure, args) }
%procedureExpr = type { i16, i16, %vectorLikeExpr*, %genericExpr* (%vectorLikeExpr*, %genericExpr*)* }

; {type, gcstate, length, data}
%bytevectorExpr = type { i16, i16, i32, i8* }

; These are defined in core/constbools.cpp in our runtime
; The availabe_externally is just telling LLVM what they contain for inlining purposees
@trueExpr = available_externally constant %booleanExpr {i16 4, i16 0, i32 1}
@falseExpr = available_externally constant %booleanExpr {i16 4, i16 0, i32 0}
