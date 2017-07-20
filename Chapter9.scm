(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else
           (eq? sorn a)))))

(define pick
  (lambda (n lat)
    (cond ((= n 1) (car lat))
          (else (pick (- n 1) (cdr lat))))))

(looking 'caviar '(6 2 grits caviar 5 7 3))
(looking 'caviar '(6 2 4 caviar 5 7 3))

(looking 'caviar '(7 1 2 caviar 5 6 3))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (cadr p)))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
(shift '((a b) c))
(shift '((a b) (c d)))

;; support from chapter7
(define a-pair?
  (lambda (x)
    (cond ((atom? x) false)
          ((null? x) false)
          ((null? (cdr x)) false)
          ((null? (cddr x)) true)
          (else false))))
;; support from chapter1
(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))
(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else
           (build (first pora)
                  (align (second pora)))))))

(align '((a b) c))
(shift '((a b) (c d)))


