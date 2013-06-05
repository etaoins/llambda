package llambda.frontend

import org.scalatest.{FunSuite,Inside,OptionValues}
import llambda._

class MacroSuite extends FunSuite with Inside with OptionValues with testutil.ExpressionHelpers {
  implicit val primitiveScope = new Scope(collection.mutable.Map(SchemePrimitives.bindings.toSeq : _*))

  test("multiple template is error") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    intercept[BadSpecialFormException] {
      bodyFor(
        """(define-syntax six
             (syntax-rules ()
             ((six)
               5
               6
           )))""")(scope)

    }
  }

  test("trivial replacement") {
    assert(expressionFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((false-literal)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.FalseLiteral))
  }
  
  test("first pattern symbol is ignored") {
    assert(expressionFor(
      """(define-syntax false-literal
           (syntax-rules ()
             ((SOMETHING-COMPLETELY-DIFFERENT)
               #f
         )))
         (false-literal)"""
    ) === et.Literal(ast.FalseLiteral))
  }
  
  test("simple expansion") {
    assert(expressionFor(
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
    assert(expressionFor(
      """(define-syntax func-returning
           (syntax-rules ()
             ((func-returning value)
               (lambda () value)
         )))
         (lambda () 
           (func-returning if))"""
    ) === et.Procedure(Nil, None, List(et.Procedure(Nil, None, List(et.VarReference(SchemePrimitives.If))))) )
  }
  
  test("two value expansion") {
    assert(expressionFor(
      """(define-syntax return-two
           (syntax-rules ()
             ((return-two a b)
               '(a . b)
         )))
         (return-two #t #f)"""
    ) === et.Literal(ast.Pair(ast.TrueLiteral, ast.FalseLiteral)))
  }
  
  test("literals must exactly match") {
    intercept[NoSyntaxRuleException] {
      expressionFor(
        """(define-syntax for
             (syntax-rules (in)
               ((for a in b)
                 '(a . b)
           )))
           (for 1 foo 2)""")
    }
  }
  
  test("ellipsis can be a literal") {
    intercept[NoSyntaxRuleException] {
      // If ellipsis wasn't a literal the middle of the list would match "b ..."
      expressionFor(
        """(define-syntax ellipsis-literal
             (syntax-rules (...)
               ((ellipsis-literal a b ... c)
                 #f
           )))
           (ellipsis-literal 10 11 12 13 14)""")
    }
  }
  
  test("multiple rules") {
    assert(expressionFor(
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
    assert(expressionFor(
      """(define-syntax recurse
           (syntax-rules ()
             ((recurse) 7)
             ((recurse _) (recurse))
         ))
         (recurse 'a)"""
    ) === et.Literal(ast.IntegerLiteral(7)))
  }
  
  test("proper list matching") {
    assert(expressionFor(
      """(define-syntax second-element
           (syntax-rules ()
             ((second-element (first second third) after) 
               '(second . after))
         ))
         (second-element (a b c) 4)"""
    ) === et.Literal(ast.Pair(ast.Symbol("b"), ast.IntegerLiteral(4))))
  }
  
  test("improper list matching") {
    assert(expressionFor(
      """(define-syntax improper-mix
           (syntax-rules ()
             ((improper-mix (var1 var2 ... . varn)) 
               '(var2 ... g varn))
         ))
         (improper-mix (a b c . d))"""
    ) === et.Literal(ast.ProperList(List(ast.Symbol("b"), ast.Symbol("c"), ast.Symbol("g"), ast.Symbol("d")))))
  }
  
  test("vector matching") {
    assert(expressionFor(
      """(define-syntax vector-mid
           (syntax-rules ()
             ((ivector-mid #(var1 var2 ... last)) 
               '(#f var2 ... #t))
         ))
         (vector-mid #(a b c d))"""
    ) === et.Literal(ast.ProperList(List(ast.FalseLiteral, ast.Symbol("b"), ast.Symbol("c"), ast.TrueLiteral))))
  }
  
  test("constant matching") {
    assert(expressionFor(
      """(define-syntax truth-symbol
           (syntax-rules ()
             ((truth-symbol #t) 'true) 
             ((truth-symbol #f) 'false) 
         ))
         (truth-symbol #t)"""
    ) === et.Literal(ast.Symbol("true")))
    
    assert(expressionFor(
      """(define-syntax truth-symbol
           (syntax-rules ()
             ((truth-symbol #t) 'true) 
             ((truth-symbol #f) 'false) 
         ))
         (truth-symbol #f)"""
    ) === et.Literal(ast.Symbol("false")))

    intercept[NoSyntaxRuleException] {
      expressionFor(
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
    assert(expressionFor(
      """(define-syntax deep-extract
           (syntax-rules ()
             ((deep-extract (1 #(first #t (_ . second)))) 
               '(first . second))
         ))
         (deep-extract (1 #(y #t (_ . z))))"""
    ) === et.Literal(ast.Pair(ast.Symbol("y"), ast.Symbol("z"))))

    intercept[NoSyntaxRuleException] {
      expressionFor(
        """(define-syntax deep-extract
             (syntax-rules ()
               ((deep-extract (1 #(first #t (_ . second)))) 
                 '(first . second))
           ))
           (deep-extract (1 #(y #f (_ . z))))""")
    }
  }
  
  test("wildcards") {
    assert(expressionFor(
      """(define-syntax return-second
           (syntax-rules ()
             ((return-second _ foo)
               foo
         )))
         (return-second 'a 'b)"""
    ) === et.Literal(ast.Symbol("b")))
  }
  
  test("first rule matches") {
    assert(expressionFor(
      """(define-syntax return-one
           (syntax-rules ()
             ((return-one foo bar) 0)
             ((return-one foobar) 1)
             ((return-one foobaz) 2)))
         (return-one #t)"""
    ) === et.Literal(ast.IntegerLiteral(1)))
  }

  test("terminal zero or more match") {
    assert(expressionFor(
      """(define-syntax return-all
           (syntax-rules ()
             ((return-all values ...)
               '(values ...)
         )))
         (return-all 1 2 3)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.IntegerLiteral(3)))))
  }
  
  test("zero or more match with zero matches") {
    assert(expressionFor(
      """(define-syntax return-all
           (syntax-rules ()
             ((return-all values ...)
               '(values ...)
         )))
         (return-all)"""
    ) === et.Literal(ast.EmptyList))
  }
  
  test("middle zero or more match") {
    assert(expressionFor(
      """(define-syntax return-all-but-first-last
           (syntax-rules ()
             ((return-all-but-first-last first values ... last)
               '(values ...)
         )))
         (return-all-but-first-last 1 2 3 4 5)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(2), ast.IntegerLiteral(3), ast.IntegerLiteral(4)))))
    
    // This requires at least two datums
    intercept[NoSyntaxRuleException] {
      expressionFor(
        """(define-syntax return-all-but-first-last
             (syntax-rules ()
               ((return-all-but-first-last first values ... last)
                 '(values ...)
           )))
           (return-all-but-first-last 'a)""")
    }
  }
  
  test("splice to middle of proper list") {
    assert(expressionFor(
      """(define-syntax append-false
           (syntax-rules ()
             ((append-false values ...)
               '(values ... #f)
         )))
         (append-false 1 2)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2), ast.FalseLiteral))))
  }
  
  test("splice to middle of improper list") {
    assert(expressionFor(
      """(define-syntax append-improper-false
           (syntax-rules ()
             ((append-improper-false values ...)
               '(values ... . #f)
         )))
         (append-improper-false 1 2)"""
    ) === et.Literal(ast.ImproperList(List(ast.IntegerLiteral(1), ast.IntegerLiteral(2)), ast.FalseLiteral)))
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

    assert(exprs == List(
      et.SetVar(scope.get("a").value, et.Literal(ast.IntegerLiteral(2)))
    ))
  }

  test("restructuring expansion") {
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
      case et.ProcedureCall(et.Procedure(arg1 :: arg2 :: Nil, None, body), argVal1 :: argVal2 :: Nil) =>
        assert(body === List(et.VarReference(arg1), et.VarReference(arg2)))
        assert(argVal1 === et.Literal(ast.IntegerLiteral(1)))
        assert(argVal2 === et.Literal(ast.IntegerLiteral(2)))
    }
  }
  
  test("dependent macros") {
    val scope = new Scope(collection.mutable.Map(), Some(primitiveScope))

    val expr = expressionFor(
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
      case et.ProcedureCall(et.Procedure(arg :: Nil, None, bodyExpr :: Nil), argVal :: Nil) =>
        assert(bodyExpr === et.Conditional(et.VarReference(arg), et.VarReference(arg), et.Literal(ast.IntegerLiteral(2))))
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
    val expr = expressionFor("(or 1 2)")(expandScope) 

    inside(expr) {
      case et.ProcedureCall(et.Procedure(arg :: Nil, None, bodyExpr :: Nil), argVal :: Nil) =>
        assert(bodyExpr === et.Conditional(et.VarReference(arg), et.VarReference(arg), et.Literal(ast.IntegerLiteral(2))))
        assert(argVal === et.Literal(ast.IntegerLiteral(1)))
    }
  }

  test("syntax-error") {
    intercept[UserDefinedSyntaxError] {
      expressionFor(
        """(define-syntax error-if-pair
             (syntax-rules ()
               ((error-if-pair (a . b))
                 (syntax-error "Did not expect pair" (a . b)))))
           (error-if-pair (1 . 2))""")
    }
  }
  
  test("escape ellipsis") {
    assert(expressionFor(
      """(define-syntax literal-ellipsis
           (syntax-rules ()
             ((literal-ellipsis values ...)
               '(... ...)
         )))
         (literal-ellipsis 1 2 3)"""
    ) === et.Literal(ast.Symbol("...")))
    
    assert(expressionFor(
      """(define-syntax ignore-ellipsis
           (syntax-rules ()
             ((ignore-ellipsis first values ...)
               '(... (first values ...))
         )))
         (ignore-ellipsis 1 2 3)"""
    ) === et.Literal(ast.ProperList(List(ast.IntegerLiteral(1), ast.Symbol("values"), ast.Symbol("...")))))
  }
}

