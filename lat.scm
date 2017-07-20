(define (lat? x)
  (cond ((null? x) true)
        ((atom? (car x)) (lat? (cdr x)))
        (else false)))