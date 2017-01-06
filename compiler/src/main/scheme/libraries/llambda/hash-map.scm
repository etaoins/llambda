(define-library (llambda hash-map)
  (import (llambda internal primitives))
  (import (llambda nfi))
  (import (llambda error))
  (import (only (scheme base) expt))

  (export hash-map? make-hash-map alist->hash-map hash-map-size hash-map-assoc hash-map-delete hash-map-exists?
          hash-map-ref/default hash-map-ref hash-map->alist hash-map-keys hash-map-values hash-map-for-each
          hash-map-fold hash-map-merge hash)

  (begin
    (define-native-library llhashmap (static-library "ll_llambda_hashmap"))

    (define-type AnyHashMap (HashMap <any> <any>))

    (define hash-map? (make-predicate AnyHashMap))

    (define make-hash-map (world-function llhashmap "llhashmap_make_hash_map" (-> (HashMap (U) (U)))))
    (define alist->hash-map (world-function llhashmap "llhashmap_alist_to_hash_map" (All (K V) (Listof (Pairof K V)) (HashMap K V))))

    (define hash-map-assoc (world-function llhashmap "llhashmap_hash_map_assoc" (All (K V) (HashMap K V) K V (HashMap K V))))
    (define hash-map-delete (world-function llhashmap "llhashmap_hash_map_delete" (All (K V) (HashMap K V) <any> (HashMap K V))))

    (define hash-map-size (native-function llhashmap "llhashmap_hash_map_size" (-> AnyHashMap <native-int64>) nocapture))
    (define hash-map-exists? (native-function llhashmap "llhashmap_hash_map_exists" (-> AnyHashMap <any> <native-bool>) nocapture))
    (define hash-map-ref/default (native-function llhashmap "llhashmap_hash_map_ref_default" (All (V) (HashMap <any> V) <any> V V)))

    (define native-hash-map-ref (world-function llhashmap "llhashmap_hash_map_ref" (All (V) (HashMap <any> V) <any> (-> V) V)))

    (: hash-map-ref (All (V) (->* ((HashMap <any> V) <any>) ((-> V)) V)))
    (define (hash-map-ref hash-map key [thunk (lambda () (raise-invalid-argument-error "Key does not exist in hash" key))])
      (native-hash-map-ref hash-map key thunk))

    (define hash-map->alist (world-function llhashmap "llhashmap_hash_map_to_alist" (All (K V) (HashMap K V) (Listof (Pairof K V)))))
    (define hash-map-keys (world-function llhashmap "llhashmap_hash_map_keys" (All (K V) (HashMap K V) (Listof K))))
    (define hash-map-values (world-function llhashmap "llhashmap_hash_map_values" (All (K V) (HashMap K V) (Listof V))))

    (define hash-map-for-each (world-function llhashmap "llhashmap_hash_map_for_each" (-> (-> <any> <any> <unit>) AnyHashMap <unit>)))
    (define hash-map-fold (world-function llhashmap "llhashmap_hash_map_fold" (All (A) (-> <any> <any> <any> A) A AnyHashMap A)))
    (define hash-map-merge (world-function llhashmap "llhashmap_hash_map_merge" (All (K V) (-> (HashMap K V) (HashMap K V) (HashMap K V)))))

    (define native-hash (native-function llhashmap "llhashmap_hash" (-> <any> <native-int64> <native-uint32>) nocapture))
    (define (hash value [bound : <integer> (expt 2 32)])
      (native-hash value bound))))
