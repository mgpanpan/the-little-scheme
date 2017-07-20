(define (member? x lat)
  (cond ((null? lat) false)
        (else (or (eq? x (car lat))
                  (member? x (cdr lat))))))
(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
(member? 'tea '(coffee (tea) or milk))
(member? 'meat '(mashed potatoes meat gravy))

;; my version
(define (member? x lat)
  (cond ((null? lat) false)
        ((eq? x (car lat)) true)
        (else (member? x (cdr lat)))))
(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
(member? 'tea '(coffee (tea) or milk))
(member? 'meat '(mashed potatoes meat gravy))

