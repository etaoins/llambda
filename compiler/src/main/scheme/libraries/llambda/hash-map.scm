(define-library (llambda hash-map)
  (import (llambda internal primitives))
  (import (llambda nfi))
  (import (llambda error))

  (export hash-map? make-hash-map alist->hash-map hash-map-size hash-map-assoc hash-map-delete hash-map-exists?
          hash-map-ref/default)

  (begin
    (define-native-library llhashmap (static-library "ll_llambda_hashmap"))

    (define-type AnyHashMap (HashMap <any> <any>))

    (define hash-map? (make-predicate AnyHashMap))

    (define make-hash-map (world-function llhashmap "llhashmap_make_hash_map" (-> (HashMap (U) (U)))))
    (define alist->hash-map (world-function llhashmap "llhashmap_alist_to_hash_map" (All (K V) (Listof (Pairof K V)) (HashMap K V))))

    (define hash-map-assoc (world-function llhashmap "llhashmap_hash_map_assoc" (All (K V) (HashMap K V) K V (HashMap K V))))
    (define hash-map-delete (world-function llhashmap "llhashmap_hash_map_delete" (All (K V) (HashMap K V) <any> (HashMap K V))))

    (define hash-map-size (native-function llhashmap "llhashmap_hash_map_size" (-> AnyHashMap <native-int64>)))
    (define hash-map-exists? (native-function llhashmap "llhashmap_hash_map_exists" (-> AnyHashMap <any> <native-bool>)))
    (define hash-map-ref/default (native-function llhashmap "llhashmap_hash_map_ref_default" (All (V) (HashMap <any> V) <any> V V)))))
