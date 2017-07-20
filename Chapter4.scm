(define (add1 n)
  (+ n 1))
(define (sub1 n)
  (- n 1))
(define (o+ n m)
  (cond ((zero? m) n)
        (else (add1 (o+ n (sub1 m))))))
(define (o- n m)
  (cond ((zero? m) n)
        (else (sub1 (o- n (sub1 m))))))
(o- 14 3)
(o- 17 9)
(o- 18 25)

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else
           (o+ (car tup)
               (addtup (cdr tup)))))))
(addtup '(1 2 3 4 5))
(addtup (list 1 2 3 4 5))

(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (+ n (o* n (sub1 m)))))))
(o* 5 3)
(o* 13 4)

(define tup+
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) '())
          (else
           (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6))
(tup+ '(3 7) '(4 6 8 1))  ;; error for the above version

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else
           (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 7) '(4 6 8 1))

(define o>
  (lambda (n m)
    (cond ((zero? n) false)
          ((zero? m) true)
          (else (o> (sub1 n) (sub1 m))))))
(o> 12 133)
(o> 120 11)
(o> 3 3)

(define o<
  (lambda (n m)
    (cond ((zero? m) false)
          ((zero? n) true)
          (else (o< (sub1 n) (sub1 m))))))
(o< 4 6)
(o< 8 3)
(o< 6 6)

(define o=
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? n) false)
          (else (o= (sub1 n) (sub1 m))))))
(o= 1 2)
(o= 1 1)
(o= 100 100)

(define o=
  (lambda (n m)
    (cond ((> n m) false)
          ((< n m) false)
          (else true))))
(o= 1 2)
(o= 1 1)
(o= 100 100)

(define o-expt
  (lambda (n m)
    (cond ((zero? m) 1)
          (else
           (o* n (o-expt n (sub1 m)))))))
(o-expt 1 1)
(o-expt 2 3)
(o-expt 5 3)

(define o/
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (o/ (- n m) m))))))
(o/ 1 2)
(o/ 15 4)

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (length (cdr lat)))))))
(length '(hotdogs with mustard sauerkraut and pickles))
(length '(ham nad cheese on rye))

;; begin from 1
(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else
           (pick (sub1 n) (cdr lat))))))
(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
;; (pick 0 '(lasagna spaghetti ravioli macaroni meatball))
(pick 3 '(lasagna spaghetti ravioli macaroni meatball))

;; begin from 0
(define pick
  (lambda (n lat)
    (cond ((zero? n) (car lat))
          (else
           (pick (sub1 n) (cdr lat))))))
(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
(pick 0 '(lasagna spaghetti ravioli macaroni meatball))
(pick 3 '(lasagna spaghetti ravioli macaroni meatball))
;; (pick 5 '(lasagna spaghetti ravioli macaroni meatball))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
          (else
           (cons (car lat)
                 (rempick (sub1 n)
                          (cdr lat)))))))
(rempick 3 '(hotdogs with hot mustard))
(rempick 1 '(hotdogs with hot mustard))
(rempick 4 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
          (else
           (cond ((number? (car lat))
                  (no-nums (cdr lat)))
                 (else
                  (cons (car lat)
                        (no-nums (cdr lat)))))))))
(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat))
           (cons (car lat)
                 (all-nums (cdr lat))))
          (else
           (all-nums (cdr lat))))))
(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
          ((or (number? a1) (number? a2)) false)
          (else
           (eq? a1 a2)))))
(eqan? 'a 'b)
(eqan? 'a 'a)
(eqan? 1 2)
(eqan? 1 1)
(eqan? 1 'a)

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eq? a (car lat))
           (add1 (occur a (cdr lat))))
          (else
           (occur a (cdr lat))))))
(occur 'a '(a b c d))
(occur 'a '(a bc d a b e f a c))
(occur 'b '(a bc d a b e f a c))
(occur 'bc '(a bc d a b e f a c))
(occur 'e '(a bc d a b e f a c))

(define one?
  (lambda (n)
    (o= n 1)))

;; begin from 1
(define rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else
           (cons (car lat)
                 (rempick (sub1 n)
                          (cdr lat)))))))
(rempick 3 '(hotdogs with hot mustard))
(rempick 1 '(hotdogs with hot mustard))
(rempick 4 '(hotdogs with hot mustard))

