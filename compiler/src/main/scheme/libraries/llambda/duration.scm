(define-library (llambda duration)
	(import (llambda base))
	(import (llambda typed))

  (export microseconds milliseconds seconds minutes hours <duration>)

  (begin
    (define-type <duration> <integer>)

    (: microseconds (-> <number> <duration>))
    (define (microseconds x)
      (integer (round x)))

    (: milliseconds (-> <number> <duration>))
    (define (milliseconds x)
      (microseconds (* x 1000)))

    (: seconds (-> <number> <duration>))
    (define (seconds x)
      (milliseconds (* x 1000)))

    (: minutes (-> <number> <duration>))
    (define (minutes x)
      (seconds (* x 60)))

    (: hours (-> <number> <duration>))
    (define (hours x)
      (minutes (* x 60)))))
