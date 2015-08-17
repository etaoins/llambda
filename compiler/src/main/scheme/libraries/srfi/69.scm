(define-library (srfi 69)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme char))
  (import (llambda hash-map))
  (import (llambda typed))

  (export make-hash-table hash-table? hash-table-equivalence-function hash-table-hash-function alist->hash-table
          hash-table-ref hash-table-ref/default hash-table-set! hash-table-delete! hash-table-exists?
          hash-table-update! hash-table-update!/default hash-table-size hash-table-keys hash-table-values
          hash-table-walk hash-table-fold hash-table->alist hash-table-copy hash-table-merge! hash string-hash
          string-ci-hash hash-by-identity)

  (begin
    (define-record-type <hash-table> (make-hash-table-internal hash-map) hash-table?
                        ([hash-map : (HashMap <any> <any>)] hash-table-hash-map set-hash-table-hash-map!))

    (define (make-hash-table) (make-hash-table-internal (make-hash-map)))
    (define (alist->hash-table [alist : (Listof <any>)])
      (make-hash-table-internal (alist->hash-map alist)))

    ; (llambda hash-table) does not support custom equivalence or hash functions
    (define (hash-table-equivalence-function [ht : <hash-table>]) equal?)
    (define (hash-table-hash-function [ht : <hash-table>]) hash)

    (define hash-table-ref
      (case-lambda
        (([hash-table : <hash-table>] key)
         (hash-map-ref (hash-table-hash-map hash-table) key))
        (([hash-table : <hash-table>] key [thunk : (-> <any>)])
         (hash-map-ref (hash-table-hash-map hash-table) key thunk))))

    (define (hash-table-ref/default [hash-table : <hash-table>] key default)
      (hash-map-ref/default (hash-table-hash-map hash-table) key default))

    (define (hash-table-set! [hash-table : <hash-table>] key value)
      (define new-hash-map (hash-map-assoc (hash-table-hash-map hash-table) key value))
      (set-hash-table-hash-map! hash-table new-hash-map)
      hash-table)

    (define (hash-table-delete! [hash-table : <hash-table>] key)
      (define new-hash-map (hash-map-delete (hash-table-hash-map hash-table) key))
      (set-hash-table-hash-map! hash-table new-hash-map)
      hash-table)

    (define (hash-table-exists? [hash-table : <hash-table>] key)
      (hash-map-exists? (hash-table-hash-map hash-table) key))

    (define hash-table-update!
      (case-lambda
        (([hash-table : <hash-table>] key [func : (-> <any> <any>)])
         (define existing-hash-map (hash-table-hash-map hash-table))
         (define existing-value (hash-map-ref existing-hash-map key))

         (define new-hash-map (hash-map-assoc existing-hash-map key (func existing-value)))
         (set-hash-table-hash-map! hash-table new-hash-map)
         hash-table)
        (([hash-table : <hash-table>] key [func : (-> <any> <any>)] [thunk : (-> <any>)])
         (define existing-hash-map (hash-table-hash-map hash-table))
         (define existing-value (hash-map-ref existing-hash-map key thunk))

         (define new-hash-map (hash-map-assoc existing-hash-map key (func existing-value)))
         (set-hash-table-hash-map! hash-table new-hash-map)
         hash-table)))

    (define (hash-table-update!/default [hash-table : <hash-table>] key [func : (-> <any> <any>)] default)
      (define existing-hash-map (hash-table-hash-map hash-table))
      (define existing-value (hash-map-ref/default existing-hash-map key default))

      (define new-hash-map (hash-map-assoc existing-hash-map key (func existing-value)))
      (set-hash-table-hash-map! hash-table new-hash-map)
      hash-table)

    (define (hash-table-size [hash-table : <hash-table>])
      (hash-map-size (hash-table-hash-map hash-table)))

    (define (hash-table-keys [hash-table : <hash-table>])
      (hash-map-keys (hash-table-hash-map hash-table)))

    (define (hash-table-values [hash-table : <hash-table>])
      (hash-map-values (hash-table-hash-map hash-table)))

    (define (hash-table-walk [hash-table : <hash-table>] [proc : (-> <any> <any> <unit>)])
      (hash-map-for-each proc (hash-table-hash-map hash-table)))

    (define (hash-table-fold [hash-table : <hash-table>] [proc : (-> <any> <any> <any> <any>)] init-value)
      (hash-map-fold proc init-value (hash-table-hash-map hash-table)))

    (define (hash-table->alist [hash-table : <hash-table>])
      (hash-map->alist (hash-table-hash-map hash-table)))

    (define (hash-table-copy [hash-table : <hash-table>])
      (make-hash-table-internal (hash-table-hash-map hash-table)))

    (define (hash-table-merge! [hash-table-1 : <hash-table>] [hash-table-2 : <hash-table>])
      (define merged-hash-map (hash-map-merge (hash-table-hash-map hash-table-1) (hash-table-hash-map hash-table-2)))
      (make-hash-table-internal merged-hash-map))

    (define (string-hash [str : <string>]) (hash str))
    (define (string-ci-hash [str : <string>]) (hash (string-foldcase str)))
    (define hash-by-identity hash)))
