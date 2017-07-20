(define rember-f
  (lambda (test? a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else
           (cons (car l) (rember-f test? a (cdr l)))))))
(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)
(eq?-salad 'tuna)
((eq?-c 'salad) 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? a (car l)) (cdr l))
            (else
             (cons (car l) ((rember-f test?) a (cdr l))))))))
((rember-f eq?) 'tuna '(tuna salad is good))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons new l))
            (else
             (cons (car l) ((insertL-f test?) new old (cdr l))))))))
((insertL-f eq?) 'new 'tuna '(tuna salad is good))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons old (cons new (cdr l))))
            (else
             (cons (car l) ((insertR-f test?) new old (cdr l))))))))
((insertR-f eq?) 'new 'tuna '(tuna salad is good))

;; my version
(define insert-g
  (lambda (position)
    (lambda (test?)
      (lambda (new old l)
        (cond ((null? l) '())
              ((test? old (car l))
               (cond ((eq? position 'left)
                      (cons new l))
                     ((eq? position 'right)
                      (cons old (cons new (cdr l))))
                     (else
                      (error "Error position"))))
              (else
               (cons (car l) (((insert-g position) test?) new old l))))))))
(((insert-g 'left) eq?) 'new 'tuna '(tuna salad is good))
(((insert-g 'right) eq?) 'new 'tuna '(tuna salad is good))

;; support from Chapter6
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
;; support from Chapter1
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(value '(+ 3 (* 3 5)))
(value 13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))
(value 'cookie)

(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) +)
          ((eq? x '*) *)
          ((eq? x '^) expt)
          (else (error "operator not defined" x)))))
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else
           ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))
(value '(+ 3 (* 3 5)))
(value 13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))
(value 'cookie)

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
           (col '() '()))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat)
                                  seen)))))))
(multirember&co 'tuna '(strawberries tuna and swordfish)
                (lambda (x y)
                  (null? y)))
(multirember&co 'tuna '()
                (lambda (x y)
                  (null? y)))
(multirember&co 'tuna '(tuna)
                (lambda (x y)
                  (null? y)))
(multirember&co 'tuna '(strawberries tuna and swordfish)
                (lambda (x y)
                  (length x)))
(define show-process
  (lambda (x y)
    (display x)
    (newline)
    (display y)))
(multirember&co 'tuna '(strawberries tuna and swordfish tuna and tuna)
                show-process)

;; support from Chapter3
(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (cons old
                           (multiinsertL new old (cdr lat)))))
          (else
           (cons (car lat)
                 (multiinsertL new old (cdr lat)))))))
(multiinsertL 'tuna 'and '(strawberries and swordfish and))
(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old (cons new
                           (multiinsertR new old (cdr lat)))))
          (else
           (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))
(multiinsertR 'tuna 'and '(strawberries and swordfish and))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new (cons oldL
                           (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR)
           (cons oldR (cons new
                            (multiinsertLR new oldL oldR (cdr lat)))))
          (else
           (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
(multiinsertLR 'tuna 'and 'and '(strawberries and swordfish and)) ;; not we want
(multiinsertLR 'tuna 'and 'or '(strawberries and or swordfish and fish or chips))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL)
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons new (cons oldL newlat))
                                    (+ L 1) R))))
          ((eq? (car lat) oldR)
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons oldR (cons new newlat))
                                    L (+ R 1)))))
          (else
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat)
                                    L R)))))))
(define show-result
  (lambda (lat t1 t2)
    (newline)
    (display "result: ")
    (display lat)
    (newline)
    (display "insert left times: ")
    (display t1)
    (newline)
    (display "insert right times: ")
    (display t2)
    ))
(multiinsertLR&co 'salty 'fish 'chips
                  '(chips and fish or fish and chips and fish)
                  show-result)

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l))
                  (cons (car l)
                        (evens-only* (cdr l))))
                 (else
                  (evens-only* (cdr l)))))
          (else
           (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 0 1))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co (cdr l)
                                  (lambda (newlat sum-odd mul-even)
                                    (col (cons (car l) newlat)
                                         sum-odd (* mul-even (car l))))))
                 (else
                  (evens-only*&co (cdr l)
                                  (lambda (newlat sum-odd mul-even)
                                    (col newlat
                                         (+ sum-odd (car l)) mul-even))))))
          (else
           (evens-only*&co (car l)
                           (lambda (al as ap)
                             (evens-only*&co (cdr l)
                                             (lambda (dl ds dp)
                                               (col (cons al dl)
                                                    (+ as ds)
                                                    (* ap dp))))))))))
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
