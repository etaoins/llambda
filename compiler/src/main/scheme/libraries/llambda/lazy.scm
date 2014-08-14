(define-library (llambda lazy)
  (import (scheme base))
	(import (llambda internal primitives))
  (import (llambda nfi))

  ; R7RS lazy library
  (include-library-declarations "../../interfaces/scheme/lazy.scm")
  ; Also export the <promise> type if we're included directly
  (export <promise>)

  (begin
    (define-record-type: <promise-data> (make-promise-data done? value) promise-data?
      ((done? : <boolean>) promise-data-done? set-promise-data-done!)
      ((value : <any>) promise-data-value set-promise-data-value!))

    (define-record-type: <promise> (make-promise-box data) promise?
      ((data : <promise-data>) promise-data set-promise-data!))

    ; Shorthand for creating a promise box with initial promise data
    ; This is is also compatible the the procedure of the same name in the R7RS example implementation
    (define-syntax make-promise-internal
      (syntax-rules ()
                    ((make-promise-internal done? value)
                     (make-promise-box (make-promise-data done? value)))))

    (define-syntax delay-force
      (syntax-rules ()
                    ((delay-force expression)
                     (make-promise-internal #f (lambda () expression)))))

    (define-syntax delay
      (syntax-rules ()
                    ((delay expression)
                     (delay-force (make-promise-internal #t expression)))))

    (define (make-promise value)
      (make-promise-internal #t value))

    (define (promise-done? x)
      (promise-data-done? (promise-data x)))

    (define (promise-value x)
      (promise-data-value (promise-data x)))

    (define (promise-update! new old)
      (set-promise-data-done! (promise-data old) (promise-done? new))
      (set-promise-data-value! (promise-data old) (promise-value new))
      (set-promise-data! new (promise-data old)))

    (define (force promise)
      (if (promise-done? promise)
        (promise-value promise)
        (let ((promise* ((promise-value promise))))
          (unless (promise-done? promise)
            (promise-update! promise* promise))
          (force promise)))))
)
