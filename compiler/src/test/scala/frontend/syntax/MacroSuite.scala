package io.llambda.compiler.frontend.syntax
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda.compiler._

class MacroSuite extends FunSuite with Inside with OptionValues with testutil.ExprHelpers {
  val primitiveScope = new Scope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

  test("multiple template is error") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax six
             (syntax-rules ()
             ((six)
               5
               6
           )))""")(syntaxScope)
    }
  }

  test("trivial replacement") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((false-literal)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.BooleanLiteral(false)))
  }

  test("redefine syntax fails in llambda") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[DuplicateDefinitionException] {
      bodyFor(
        """(define-syntax false-literal
             (syntax-rules ()
               ((false-literal)
                 #f
           )))
           (define-syntax false-literal
             (syntax-rules ()
               ((false-literal)
                 #f
           )))
           (false-literal)""")(syntaxScope)
    }
  }
  
  test("syntax cannot be used as expression") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[MalformedExprException] {
      exprFor(
        """(define-syntax false-literal
             (syntax-rules ()
               ((false-literal)
                 #f
           )))
           false-literal""")
    }
  }
  
  test("first pattern symbol is ignored") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((SOMETHING-COMPLETELY-DIFFERENT)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.BooleanLiteral(false)))
  }
  
  test("simple expansion") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-single
           (syntax-rules ()
             ((return-single foo)
               foo
         )))
         (return-single 6)"""
    ) === et.Literal(ast.IntegerLiteral(6)))
  }
  
  test("expansion with lambdas") {
    // This expands a macro containing a lambda inside a lambda
    // We had a bug with rescoping and became confused while rescoping a lambda
    // body that contained symbols from a different scope. This triggered it.
    inside(exprFor(
      """(define-syntax func-returning
           (syntax-rules ()
             ((func-returning value)
               (lambda () value)
         )))
         (lambda () 
           (func-returning +))"""
    )(plusScope)) {
      case et.Lambda(_, Nil, Nil, None, et.Lambda(_, Nil, Nil, None, et.VarRef(`plusLoc`), Some(_)), Some(_)) =>
    }
  }

  test("macro inside lambda shadows outer macro") {
    // This is to ensure when we attempt to split our body definition in to bindings and body expressions we don't get
    // overeager and expand using the outer macro definition
    inside(exprFor(
      """(define-syntax shadowed-macro
           (syntax-rules ()
             ((shadowed-macro)
               'outer)))
         (lambda ()
           (define-syntax shadowed-macro
             (syntax-rules ()
               ((shadowed-macro)
                 'inner)))
           (shadowed-macro))"""
    )(plusScope)) {
      case et.Lambda(_, Nil, Nil, None, et.Literal(ast.Symbol("inner")), Some(_)) =>
    }
  }

  test("two value expansion") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-two
           (syntax-rules ()
             ((return-two a b)
               '(a . b)
         )))
         (return-two #t #f)"""
    ) === et.Literal(ast.Pair(ast.BooleanLiteral(true), ast.BooleanLiteral(false))))
  }
  
  test("literals must exactly match") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[NoSyntaxRuleException] {
      exprFor(
        """(define-syntax for
             (syntax-rules (in)
               ((for a in b)
                 '(a . b)
           )))
           (for 1 foo 2)""")
    }
  }

  test("literal operand identifiers don't match non-literal pattern identifiers") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[NoSyntaxRuleException] {
      // "literal" should not match "first-capture" 
      exprFor(
        """(define-syntax literal-test
           (syntax-rules (literal)
             ((literal-test (first-capture literal))
               first-capture)))
           (literal-test (literal literal))""")
    }
  }
  
  test("operands bound to a value don't match unbound literals") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    bodyFor(
      """(define-syntax literal-test
         (syntax-rules (literal)
           ((literal-test (literal)) 
             1)))"""
    )(syntaxScope)

    val expandScope = new Scope(collection.mutable.Map("literal-test" -> syntaxScope.get("literal-test").value), Some(primitiveScope))
    intercept[NoSyntaxRuleException] {
      bodyFor(
        """(define literal 1)
           (literal-test (literal))"""
      )(expandScope)
    }
  }
  
  test("ellipsis can be a literal") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[NoSyntaxRuleException] {
      // If ellipsis wasn't a literal the middle of the list would match "b ..."
      exprFor(
        """(define-syntax ellipsis-literal
             (syntax-rules (...)
               ((ellipsis-literal a b ... c)
                 #f
           )))
           (ellipsis-literal 10 11 12 13 14)""")
    }
  }
  
  test("multiple rules") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax arg-count
           (syntax-rules ()
             ((arg-count) 0)
             ((arg-count _) 1)
             ((arg-count _ _) 2)
         ))
         (arg-count 1.0)"""
    ) === et.Literal(ast.IntegerLiteral(1)))
  }
  
  test("recursive expansion") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax recurse
           (syntax-rules ()
             ((recurse) 7)
             ((recurse _) (recurse))
         ))
         (recurse 'a)"""
    ) === et.Literal(ast.IntegerLiteral(7)))
  }
  
  test("proper list matching") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax second-element
           (syntax-rules ()
             ((second-element (first second third) after) 
               '(second . after))
         ))
         (second-element (a b c) 4)"""
    ) === et.Literal(ast.Pair(ast.Symbol("b"), ast.IntegerLiteral(4))))
  }
  
  test("exact improper list matching") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax improper-mix
           (syntax-rules ()
             ((improper-mix (var1 var2 ... . varn)) 
               '(var2 ... g varn))
         ))
         (improper-mix (a b c . d))"""
    ) === et.Literal(ast.ProperList(List(ast.Symbol("b"), ast.Symbol("c"), ast.Symbol("g"), ast.Symbol("d")))))
  }
  
  test("improper list can match larger proper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax improper-match
           (syntax-rules ()
             ((improper-match (var1 var2 . rest)) 
               '(var2 rest var1))
         ))
         (improper-match (a b c d))"""
    ) === et.Literal(ast.ProperList(List(
      ast.Symbol("b"),
      ast.ProperList(List(
        ast.Symbol("c"),
        ast.Symbol("d")
      )),
      ast.Symbol("a")
    ))))
  }
  
  test("improper list can match larger improper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax improper-match
           (syntax-rules ()
             ((improper-match (var1 var2 . rest)) 
               '(var2 rest var1))
         ))
         (improper-match (a b c . d))"""
    ) === et.Literal(ast.ProperList(List(
      ast.Symbol("b"),
      ast.Pair(
        ast.Symbol("c"),
        ast.Symbol("d")
      ),
      ast.Symbol("a")
    ))))
  }
  
  test("pattern can be an improper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax improper-match
           (syntax-rules ()
             ((improper-match var1 var2 . rest) 
               '(var2 rest var1))
         ))
         (improper-match a b c d)"""
    ) === et.Literal(ast.ProperList(List(
      ast.Symbol("b"),
      ast.ProperList(List(
        ast.Symbol("c"),
        ast.Symbol("d")
      )),
      ast.Symbol("a")
    ))))
  }
  
  test("macro can be applied as an improper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax improper-match
           (syntax-rules ()
             ((improper-match var1 var2 . rest) 
               '(var2 rest var1))
         ))
         (improper-match a b . c)"""
    ) === et.Literal(ast.ProperList(List(
      ast.Symbol("b"),
      ast.Symbol("c"),
      ast.Symbol("a")
    ))))
  }
  
  test("vector matching") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax vector-mid
           (syntax-rules ()
             ((ivector-mid #(var1 var2 ... last)) 
               '(#f var2 ... #t))
         ))
         (vector-mid #(a b c d))"""
    ) === et.Literal(ast.ProperList(List(ast.BooleanLiteral(false), ast.Symbol("b"), ast.Symbol("c"), ast.BooleanLiteral(true)))))
  }
  
  test("constant matching") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax truth-symbol
           (syntax-rules ()
             ((truth-symbol #t) 'true) 
             ((truth-symbol #f) 'false) 
         ))
         (truth-symbol #t)"""
    ) === et.Literal(ast.Symbol("true")))
    
    assert(exprFor("(truth-symbol #f)") === et.Literal(ast.Symbol("false")))

    intercept[NoSyntaxRuleException] {
      exprFor("""(truth-symbol 6)""")
    }
  }
  
  test("deeply nested matching") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax deep-extract
           (syntax-rules ()
             ((deep-extract (1 #(first #t (_ . second)))) 
               '(first . second))
         ))
         (deep-extract (1 #(y #t (_ . z))))"""
    ) === et.Literal(ast.Pair(ast.Symbol("y"), ast.Symbol("z"))))

    intercept[NoSyntaxRuleException] {
      exprFor("(deep-extract (1 #(y #f (_ . z))))")
    }
  }
  
  test("wildcards") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-second
           (syntax-rules ()
             ((return-second _ foo)
               foo
         )))
         (return-second 'a 'b)"""
    ) === et.Literal(ast.Symbol("b")))
  }

  test("duplicate pattern variables fail at define time") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand expr expr ...)
                    ((lambda () expr ...)))))"""
      )(syntaxScope)
    }
  }
  
  test("first rule matches") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-one
           (syntax-rules ()
             ((return-one foo bar) 0)
             ((return-one foobar) 1)
             ((return-one foobaz) 2)))
         (return-one #t)"""
    ) === et.Literal(ast.IntegerLiteral(1)))
  }

  test("terminal zero or more match") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-all
           (syntax-rules ()
             ((return-all values ...)
               '(values ...)
         )))
         (return-all 1 2 3)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.IntegerLiteral(3)))))
  }
  
  test("zero or more match with zero matches") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-all
           (syntax-rules ()
             ((return-all values ...)
               '(values ...)
         )))
         (return-all)"""
    ) === et.Literal(ast.EmptyList()))
  }
  
  test("middle zero or more match") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax return-all-but-first-last
           (syntax-rules ()
             ((return-all-but-first-last first values ... last)
               '(values ...)
         )))
         (return-all-but-first-last 1 2 3 4 5)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(2), ast.IntegerLiteral(3), ast.IntegerLiteral(4)))))
    
    // This requires at least two datums
    intercept[NoSyntaxRuleException] {
      exprFor("(return-all-but-first-last 'a)")
    }
  }
  
  test("splice to middle of proper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax append-false
           (syntax-rules ()
             ((append-false values ...)
               '(values ... #f)
         )))
         (append-false 1 2)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.BooleanLiteral(false)))))
  }
  
  test("splice to middle of improper list") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax append-improper-false
           (syntax-rules ()
             ((append-improper-false values ...)
               '(values ... . #f)
         )))
         (append-improper-false 1 2)"""
    ) === et.Literal(ast.AnyList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2)), ast.BooleanLiteral(false))))
  }
  
  test("splice to middle of vector") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax append-vector-false
           (syntax-rules ()
             ((append-improper-false values ...)
               #(values ... #f)
         )))
         (append-vector-false 1 2)"""
    ) === et.Literal(ast.VectorLiteral(Vector(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.BooleanLiteral(false)))))
  }
  
  test("custom ellipsis identifier") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax append-vector-false
           (syntax-rules my-ellipsis ()
             ((append-improper-false values my-ellipsis)
               #(values my-ellipsis #f)
         )))
         (append-vector-false 1 2 3)"""
    ) === et.Literal(ast.VectorLiteral(Vector(
      ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.IntegerLiteral(3), ast.BooleanLiteral(false)
    ))))
  }

  test("zero or more template without pattern variables fails") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    // We don't have a termination condition here
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand expr ...)
                    (5 ... ))))
           (test-expand #f)"""
      )(syntaxScope)
    }
  }
  
  test("zero or more template with pattern variables from separate subpatterns fails") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand (list1 ...) (list2 ...))
                     ((list1 list2) ...))))
           (test-expand (a b c) (1 2 3))"""
      )(syntaxScope)
    }
  }

  test("top-level expressions allowed in top-level context") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor(
      """(define-syntax my-define
           (syntax-rules ()
             ((my-define ident value) (define ident value))
         ))
         (my-define a 2)"""
    )(syntaxScope)

    inside(syntaxScope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(exprs == List(
          et.TopLevelDefine(
            et.Binding(`storageLoc`, et.Literal(ast.IntegerLiteral(2)))
          )
        ))
    }
  }

  test("body expressions allowed in body context") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor(
      """(define-syntax my-define
           (syntax-rules ()
             ((my-define ident value) (define ident value))
         ))
         (lambda ()
          (my-define a 2))"""
    )(syntaxScope)

    inside(exprs) {
      case List(et.Lambda(_, Nil, Nil, None,
            et.InternalDefine(List(
              et.Binding(_, et.Literal(ast.IntegerLiteral(2)))
            ), et.Begin(Nil)),
          _)) =>
    }
  }

  test("non-restructuring repeating subpattern with replacement") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor(
      """(define-syntax repeating-pair
        (syntax-rules ()
                 ((repeating-pair (left right) ...)
                  '((left right) ...))))
           
        (repeating-pair (left1 right1) (left2 right2))"""
    )(syntaxScope)

    assert(expr === et.Literal(ast.ProperList(List(
      ast.ProperList(List(ast.Symbol("left1"), ast.Symbol("right1"))),
      ast.ProperList(List(ast.Symbol("left2"), ast.Symbol("right2")))
    ))))
  }

  test("non-restructuring repeating subpattern with no replacement") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor(
      """(define-syntax repeating-pair
         (syntax-rules ()
                  ((repeating-pair (left1 right1) (left2 right2) ...)
                   '((left1 right1)
                    (left2 right2) ...))))
           
         (repeating-pair (left1 right1))"""
    )(syntaxScope)

    assert(expr === et.Literal(ast.ProperList(List(
      ast.ProperList(List(ast.Symbol("left1"), ast.Symbol("right1")))
    ))))
  }

  test("restructuring subpattern") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr :: Nil = bodyFor(
      """(define-syntax let
           (syntax-rules ()
             ((let ((name val) ...) body1 body2 ...)
               ((lambda (name ...) body1 body2 ...)
                 val ...))))
         (let ((a 1) (b 2)) a b)"""
    )(syntaxScope) 

    inside(expr) {
      case et.Apply(et.Lambda(_, List(arg1, arg2), Nil, None, body, _), List(argVal1, argVal2)) =>
        assert(body === et.Begin(List(et.VarRef(arg1), et.VarRef(arg2))))
        assert(argVal1 === et.Literal(ast.IntegerLiteral(1)))
        assert(argVal2 === et.Literal(ast.IntegerLiteral(2)))
    }
  }

  test("nested subpatterns") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax nested-subpatterns
            (syntax-rules ()
                     ((nested-subpatterns (first second rest ...) ...)
                      #((rest ... second first) ...))))

          (nested-subpatterns (1 2 3 4) (5 6 7) (8 9))"""
    ) === et.Literal(ast.VectorLiteral(Vector(
        ast.ProperList(List(3, 4, 2, 1).map(ast.IntegerLiteral(_))),
        ast.ProperList(List(7, 6, 5).map(ast.IntegerLiteral(_))),
        ast.ProperList(List(9, 8).map(ast.IntegerLiteral(_)))
      )))
    )
  }
  
  test("dependent macros") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = exprFor(
      """(define-syntax let
           (syntax-rules ()
             ((let ((name val) ...) body1 body2 ...)
               ((lambda (name ...) body1 body2 ...)
                 val ...))))

         (define-syntax or
           (syntax-rules ()
             ((or test) test)
             ((or test1 test2 ...)
               (let ((x test1))
                 (if x x (or test2 ...))))))
         
         (or 1 2)"""
    )(syntaxScope)

    inside(expr) {
      case et.Apply(et.Lambda(_, List(arg), Nil, None, bodyExpr, _), List(argVal)) =>
        assert(bodyExpr === et.Cond(et.VarRef(arg), et.VarRef(arg), et.Literal(ast.IntegerLiteral(2))))
        assert(argVal === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("expanded macros can access their original scope") {
    val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))
    
    bodyFor(
      """(define-syntax let
           (syntax-rules ()
             ((let ((name val) ...) body1 body2 ...)
               ((lambda (name ...) body1 body2 ...)
                 val ...))))

         (define-syntax or
           (syntax-rules ()
             ((or test) test)
             ((or test1 test2 ...)
               (let ((x test1))
                 (if x x (or test2 ...))))))"""
    )(syntaxScope)

    val expandScope = new Scope(collection.mutable.Map("or" -> syntaxScope.get("or").value))
    val expr = exprFor("(or 1 2)")(expandScope) 

    inside(expr) {
      case et.Apply(et.Lambda(_, List(arg), Nil, None, bodyExpr, _), List(argVal)) =>
        assert(bodyExpr === et.Cond(et.VarRef(arg), et.VarRef(arg), et.Literal(ast.IntegerLiteral(2))))
        assert(argVal === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("syntax-error") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[UserDefinedSyntaxError] {
      exprFor(
        """(define-syntax error-if-pair
             (syntax-rules ()
               ((error-if-pair (a . b))
                 (syntax-error "Did not expect pair" (a . b)))))
           (error-if-pair (1 . 2))""")
    }
  }
  
  test("escape ellipsis") {
    implicit val syntaxScope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    assert(exprFor(
      """(define-syntax literal-ellipsis
           (syntax-rules ()
             ((literal-ellipsis values ...)
               '(... ...)
         )))
         (literal-ellipsis 1 2 3)"""
    ) === et.Literal(ast.Symbol("...")))
    
    assert(exprFor(
      """(define-syntax ignore-ellipsis
           (syntax-rules ()
             ((ignore-ellipsis first values ...)
               '(... (first values ...))
         )))
         (ignore-ellipsis 1 2 3)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.Symbol("values"), ast.Symbol("...")))))
  }
}

