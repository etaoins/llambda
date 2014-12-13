(define-library (llambda list)
  (import (scheme base))
  (import (llambda typed))
  (import (llambda nfi))

  (export cons* partition filter remove find find-tail take-while drop-while)

  ; WeakListof is only a strong type if pair are immutable
  ; This is used avoid producing type checking causing tail recursive procedures to have extremely poor performance
  (cond-expand
    (immutable-pairs
      (begin
        (define-type (WeakListof A) (Listof A))
        (define-type (WeakPairof A B) (Pairof A B))))
    (else
      (begin
        (define-type (WeakListof A) <list-element>)
        (define-type (WeakPairof A B) <pair>))))

  (begin
    (define-native-library lllist (static-library "ll_llambda_list"))

    (define cons* (world-function lllist "lllist_cons_star" (-> <any> <any> * <any>)))

    (define partition (world-function lllist "lllist_partition" (All (A) (-> <any> <boolean>) (WeakListof A) (Values (WeakListof A) (WeakListof A)))))

    ; For the passed list the head's car will be bound to "value" and have "pred?" applied. If "pred?" returns true
    ; then "true-expr" will be evaluated, otherwise "false-expr". The result of the condition will be returned
    ; If the list is empty then "empty-expr" will be evaluated and returned
    (define-syntax cond-map-head
      (syntax-rules ()
                    ((cond-map-head pred? lis value true-expr false-expr)
                     (cond-map-head pred? lis value true-expr false-expr '()))
                    ((cond-map-head pred? lis value true-expr false-expr empty-expr)
                     (if (null? lis)
                       empty-expr
                       (let ((value (car lis)))
                         (if (pred? value) true-expr false-expr))))))

    (: filter (All (A) (-> A <boolean>) (WeakListof A) (WeakListof A)))
    (define (filter pred? lis)
      (cond-map-head pred? lis value
                     (cons value (filter pred? (cdr lis)))
                     (filter pred? (cdr lis))))

    (: remove (All (A) (-> A <boolean>) (WeakListof A) (WeakListof A)))
    (define (remove pred? lis)
      (cond-map-head pred? lis value
                     (remove pred? (cdr lis))
                     (cons value (remove pred? (cdr lis)))))

    (: find (All (A) (-> A <boolean>) (WeakListof A) (U A #f)))
    (define (find pred? lis)
      (cond-map-head pred? lis value
                     value
                     (find pred? (cdr lis))
                     #f))

    (: find-tail (All (A) (-> A <boolean>) (WeakListof A) (U (WeakPairof A (WeakListof A)) #f)))
    (define (find-tail pred? lis)
      (cond-map-head pred? lis value
                     lis
                     (find-tail pred? (cdr lis))
                     #f))

    (: take-while (All (A) (-> A <boolean>) (WeakListof A) (WeakListof A)))
    (define (take-while pred? lis)
      (cond-map-head pred? lis value
                     (cons value (take-while pred? (cdr lis)))
                     '()))

    (: drop-while (All (A) (-> A <boolean>) (WeakListof A) (WeakListof A)))
    (define (drop-while pred? lis)
      (cond-map-head pred? lis value
                     (drop-while pred? (cdr lis))
                     lis))))
