(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (cdr lat))
          (else
           (cons (car lat) (rember a (cdr lat)))))))
(rember 'mint '(lamb chops and mint jelly))
(rember 'cup '(coffee cup tea cup and hick cup))

(define firsts
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cons (car (car lat))
                      (firsts (cdr lat)))))))
(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
(firsts '((a b) (c d) (e f)))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons old (cons new (cdr lat))))
          (else
           (cons (car lat) (insertR new old (cdr lat)))))))
(insertR 'topping 'fudge '(ice cream with fudge for dessert))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new lat))
          (else
           (cons (car lat) (insertL new old (cdr lat)))))))
(insertL 'topping 'fudge '(ice cream with fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new (cdr lat)))
          (else
           (cons (car lat) (subst new old (cdr lat)))))))
(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new old1 old2 lat)
    (cond ((null? lat) '())
          ((or (eq? old1 (car lat)) (eq? old2 (car lat)))
           (cons new (cdr lat)))
          (else
           (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat))
           (multirember a (cdr lat)))
          (else
           (cons (car lat) (multirember a (cdr lat)))))))
(multirember 'cup '(coffee cup tea cup and hick up))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else
           (cons (car lat) (multiinsertR new old (cdr lat)))))))
(multiinsertR 'chocolate 'cup '(coffee cup tea cup and hick up))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else
           (cons (car lat) (multiinsertL new old (cdr lat)))))))
(multiinsertL 'chocolate 'cup '(coffee cup tea cup and hick up))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons new (multisubst new old (cdr lat))))
          (else
           (cons (car lat) (multisubst new old (cdr lat)))))))
(multisubst 'chocolate 'cup '(coffee cup tea cup and hick up))