package io.llambda.compiler.frontend.syntax
import io.llambda

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda.compiler._

class MacroSuite extends FunSuite with Inside with OptionValues with testutil.ExprHelpers {
  implicit val primitiveScope = new Scope(collection.mutable.Map(PrimitiveExprs.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

  test("multiple template is error") {
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax six
             (syntax-rules ()
             ((six)
               5
               6
           )))""")(primitiveScope)
    }
  }

  test("trivial replacement") {
    assert(exprFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((false-literal)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.BooleanLiteral(false)))
  }
  
  test("syntax cannot be used as expression") {
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
    assert(exprFor(
      """(define-syntax func-returning
           (syntax-rules ()
             ((func-returning value)
               (lambda () value)
         )))
         (lambda () 
           (func-returning +))"""
    )(plusScope) === et.Lambda(Nil, None, et.Lambda(Nil, None, et.VarRef(plusLoc))) )
  }
  
  test("two value expansion") {
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
    assert(exprFor(
      """(define-syntax truth-symbol
           (syntax-rules ()
             ((truth-symbol #t) 'true) 
             ((truth-symbol #f) 'false) 
         ))
         (truth-symbol #t)"""
    ) === et.Literal(ast.Symbol("true")))
    
    assert(exprFor(
      """(define-syntax truth-symbol
           (syntax-rules ()
             ((truth-symbol #t) 'true) 
             ((truth-symbol #f) 'false) 
         ))
         (truth-symbol #f)"""
    ) === et.Literal(ast.Symbol("false")))

    intercept[NoSyntaxRuleException] {
      exprFor(
        """(define-syntax truth-symbol
             (syntax-rules ()
               ((truth-symbol #t) 'true) 
               ((truth-symbol #f) 'false) 
           ))
           (truth-symbol 6)"""
      )
    }
  }
  
  test("deeply nested matching") {
    assert(exprFor(
      """(define-syntax deep-extract
           (syntax-rules ()
             ((deep-extract (1 #(first #t (_ . second)))) 
               '(first . second))
         ))
         (deep-extract (1 #(y #t (_ . z))))"""
    ) === et.Literal(ast.Pair(ast.Symbol("y"), ast.Symbol("z"))))

    intercept[NoSyntaxRuleException] {
      exprFor(
        """(define-syntax deep-extract
             (syntax-rules ()
               ((deep-extract (1 #(first #t (_ . second)))) 
                 '(first . second))
           ))
           (deep-extract (1 #(y #f (_ . z))))""")
    }
  }
  
  test("wildcards") {
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
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand expr expr ...)
                    ((lambda () expr ...)))))"""
      )(primitiveScope)
    }
  }
  
  test("first rule matches") {
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
      exprFor(
        """(define-syntax return-all-but-first-last
             (syntax-rules ()
               ((return-all-but-first-last first values ... last)
                 '(values ...)
           )))
           (return-all-but-first-last 'a)""")
    }
  }
  
  test("splice to middle of proper list") {
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
    // We don't have a termination condition here
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand expr ...)
                    (5 ... ))))
           (test-expand #f)"""
      )(primitiveScope)
    }
  }
  
  test("zero or more template with pattern variables from separate subpatterns fails") {
    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax test-expand
             (syntax-rules ()
                   ((test-expand (list1 ...) (list2 ...))
                     ((list1 list2) ...))))
           (test-expand (a b c) (1 2 3))"""
      )(primitiveScope)
    }
  }

  test("body expressions allowed in body context") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val exprs = bodyFor(
      """(define-syntax my-define
           (syntax-rules ()
             ((my-define ident value) (define ident value))
         ))
         (my-define a 2)"""
    )(scope) 

    inside(scope.get("a").value) {
      case storageLoc : StorageLocation =>
        assert(exprs == List(
          et.TopLevelDefinition(List(
            (storageLoc, et.Literal(ast.IntegerLiteral(2))) 
          ))
        ))
    }
  }
  
  test("non-restructuring repeating subpattern with replacement") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

	 val expr = exprFor(
		 """(define-syntax repeating-pair
			  (syntax-rules ()
								 ((repeating-pair (left right) ...)
								  '((left right) ...))))
           
        (repeating-pair (left1 right1) (left2 right2))"""
    )(scope)

    assert(expr === et.Literal(ast.ProperList(List(
      ast.ProperList(List(ast.Symbol("left1"), ast.Symbol("right1"))),
      ast.ProperList(List(ast.Symbol("left2"), ast.Symbol("right2")))
    ))))
  }

  test("non-restructuring repeating subpattern with no replacement") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

	 val expr = exprFor(
		 """(define-syntax repeating-pair
			  (syntax-rules ()
								 ((repeating-pair (left1 right1) (left2 right2) ...)
								  '((left1 right1)
									 (left2 right2) ...))))
           
        (repeating-pair (left1 right1))"""
    )(scope)

    assert(expr === et.Literal(ast.ProperList(List(
      ast.ProperList(List(ast.Symbol("left1"), ast.Symbol("right1")))
    ))))
  }

  test("restructuring subpattern") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr :: Nil = bodyFor(
      """(define-syntax let
           (syntax-rules ()
             ((let ((name val) ...) body1 body2 ...)
               ((lambda (name ...) body1 body2 ...)
                 val ...))))
         (let ((a 1) (b 2)) a b)"""
    )(scope) 

    inside(expr) {
      case et.Apply(et.Lambda(arg1 :: arg2 :: Nil, None, body), argVal1 :: argVal2 :: Nil) =>
        assert(body === et.Begin(List(et.VarRef(arg1), et.VarRef(arg2))))
        assert(argVal1 === et.Literal(ast.IntegerLiteral(1)))
        assert(argVal2 === et.Literal(ast.IntegerLiteral(2)))
    }
  }

  test("nested subpatterns") {
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
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

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
    )(scope)

    inside(expr) {
      case et.Apply(et.Lambda(arg :: Nil, None, bodyExpr), argVal :: Nil) =>
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
      case et.Apply(et.Lambda(arg :: Nil, None, bodyExpr), argVal :: Nil) =>
        assert(bodyExpr === et.Cond(et.VarRef(arg), et.VarRef(arg), et.Literal(ast.IntegerLiteral(2))))
        assert(argVal === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("syntax-error") {
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

