(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define rember*
  (lambda (a lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? a (car lat)) (rember* a (cdr lat)))
                 (else
                  (cons (car lat)
                        (rember* a (cdr lat))))))
          (else
           (cons (rember* a (car lat))
                 (rember* a (cdr lat)))))))
(define l
  '((coffee) cup ((tea) cup)
    (and (hick)) cup))
(rember* 'cup l)

(define insertR*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? old (car lat))
                  (cons old (cons new (insertR* new old (cdr lat)))))
                 (else
                  (cons (car lat) (insertR* new old (cdr lat))))))
          (else
           (cons (insertR* new old (car lat))
                 (insertR* new old (cdr lat)))))))
(define l
  '((how much (wood))
    could
    ((a (wood) chuck))
    (((chuck)))
    (if (a) ((wood chuck)))
    could chuck wood))
(insertR* 'roast 'chuck l)

;; support from chapter4
(define (add1 n)
  (+ n 1))
(define (sub1 n)
  (- n 1))
(define (o+ n m)
  (cond ((zero? m) n)
        (else (add1 (o+ n (sub1 m))))))

(define occur*
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((atom? (car lat))
           (cond ((eq? a (car lat))
                  (add1 (occur* a (cdr lat))))
                 (else
                  (occur* a (cdr lat)))))
          (else
           (o+ (occur* a (car lat))
               (occur* a (cdr lat)))))))
(define l
  '((banana)
    (split ((((banana ice)))
            (cream (banana))
            sherbet))
    (banana)
    (bread)
    (banana brandy)))
(occur* 'banana l)

(define subst*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? old (car lat))
                  (cons new (subst* new old (cdr lat))))
                 (else
                  (cons (car lat) (subst* new old (cdr lat))))))
          (else
           (cons (subst* new old (car lat))
                 (subst* new old (cdr lat)))))))
(subst* 'orange 'banana l)

(define insertL*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? old (car lat))
                  (cons new (cons old (insertL* new old (cdr lat)))))
                 (else
                  (cons (car lat) (insertL* new old (cdr lat))))))
          (else
           (cons (insertL* new old (car lat))
                 (insertL* new old (cdr lat)))))))
(insertL* 'orange 'banana l)

(define member*
  (lambda (a lat)
    (cond ((null? lat) false)
          ((atom? (car lat))
           (or (eq? a (car lat))
               (member* a (cdr lat))))
          (else
           (or (member* a (car lat))
               (member* a (cdr lat)))))))
(define l '((potato) (chips ((with) fish) (chips))))
(member* 'chips l)

;; support from Chapter4
(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
          ((or (number? a1) (number? a2)) false)
          (else
           (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2))
           true)
          ((or (null? l1) (null? l2))
           false)
          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) false)
          (else
           (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))
(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))
(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))
(eqlist? '(banana ((split)))
         '((banana) (split)))
(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))
;; (eqlist? 'a 'b) ;; error
;; (eqlist? 'a 'a) ;; error

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
           (eqan? s1 s2))
          ((or (atom? s1) (atom? s2))
           false)
          (else
           (eqlist? s1 s2)))))
(equal? 'a 'b)
(equal? 'a 'a)
(equal? '(strawberry ice cream)
         '(strawberry ice cream))
(equal? '(strawberry ice cream)
         '(strawberry cream ice))
(equal? '(banana ((split)))
         '((banana) (split)))
(equal? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))
(equal? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))
(equal? '() 'a)
(equal? '() '())

;; s can be any S-expression
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l)
            (rember s (cdr l)))))))

(rember '(soda) '(beef ((sausage)) (and (soda))))
(rember '((sausage)) '(beef ((sausage)) (and (soda))))
