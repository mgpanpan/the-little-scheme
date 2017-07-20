(define member?
  (lambda (a lat)
    (cond ((null? lat) false)
          (else
           (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

