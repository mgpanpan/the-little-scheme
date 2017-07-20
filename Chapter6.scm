(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((eq? (cadr aexp) '+)
           (and (numbered? (car aexp))
                (numbered? (caddr aexp))))
          ((eq? (cadr aexp) '*)
           (and (numbered? (car aexp))
                (numbered? (caddr aexp))))
          ((eq? (cadr aexp) '^)
           (and (numbered? (car aexp))
                (numbered? (caddr aexp)))))))
(numbered? '(3 + (4 * 5)))
(numbered? '(3 + (4 ^ 5)))
(numbered? '(2 * sausage))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else
           (and (numbered? (car aexp))
                (numbered? (caddr aexp)))))))
(numbered? '(3 + (4 * 5)))
(numbered? '(3 + (4 ^ 5)))
(numbered? '(2 * sausage))

;; calculate the infix representation of expression
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (cadr nexp) '+)
           (+ (value (car nexp))
              (value (caddr nexp))))
          ((eq? (cadr nexp) '*)
           (* (value (car nexp))
              (value (caddr nexp))))
          ((eq? (cadr nexp) '^)
           (expt (value (car nexp))
                 (value (caddr nexp)))))))
(value '(3 + (3 * 5)))
(value 13)
(value '(1 + 3))
(value '(1 + (3 ^ 4)))
(value 'cookie)

;; calculate the prefix representation of expression
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (car nexp) '+)
           (+ (value (cadr nexp))
              (value (caddr nexp))))
          ((eq? (car nexp) '*)
           (* (value (cadr nexp))
              (value (caddr nexp))))
          ((eq? (car nexp) '^)
           (expt (value (cadr nexp))
                 (value (caddr nexp)))))))
(value '(+ 3 (* 3 5)))
(value 13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))
(value 'cookie)

;; prefix
(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))
(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))
(define operator
  (lambda (aexp)
    (car aexp)))
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) '+)
           (+ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '*)
           (* (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '^)
           (expt (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))
(value '(+ 3 (* 3 5)))
(value 13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))
(value 'cookie)

;; infix
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))
(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))
(define operator
  (lambda (aexp)
    (cadr aexp)))
(value '(3 + (3 * 5)))
(value 13)
(value '(1 + 3))
(value '(1 + (3 ^ 4)))
(value 'cookie)

;; another expression for numbers
;; 0 : '()
;; 1 : (())
;; 2 : (() ())
;; 3 : (() () ())
;; new primitives:  sero? edd1 zub1
(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons '() n)))
(define zub1
  (lambda (n)
    (cdr n)))
(define o+
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (o+ n (zub1 m)))))))
(define zero '())
(define one (edd1 zero))
(define two (edd1 one))
(define three (edd1 two))
(define n (o+ three two))

