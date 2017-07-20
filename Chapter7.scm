;; support from Chapter2
(define member?
  (lambda (a lat)
    (cond ((null? lat) false)
          (else
           (or (equal? a (car lat))
               (member? a (cdr lat)))))))
(define set?
  (lambda (lat)
    (cond ((null? lat) true)
          ((member? (car lat) (cdr lat))
           false)
          (else
           (set? (cdr lat))))))
(set? '(apple peaches apple plum))
(set? '(apples peaches pears plums))
(set? '(apple 3 pear 4 9 apple 3 4))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          ((member? (car lat) (cdr lat))
           (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))
(makeset '(apple peach pear peach plum apple lemon peach))

;; support from Chapter3
(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((equal? a (car lat))
           (multirember a (cdr lat)))
          (else
           (cons (car lat) (multirember a (cdr lat)))))))
(multirember 'cup '(coffee cup tea cup and hick up))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          (else
            (cons (car lat)
                  (makeset (multirember (car lat) (cdr lat))))))))
(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) true)
          ((member? (car set1) set2)
           (subset? (cdr set1) set2))
          (else false))))
(define set1 '(5 chicken wings))
(define set2 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? set1 set2)
(define set1 '(4 pounds of horseradish))
(define set2 '(four pounds chicken and 5 ounces horseradish))
(subset? set1 set2)

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) true)
          (else
           (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) false)
          ((member? (car set1) set2) true)
          (else (intersect? (cdr set1) set2)))))
(define set1 '(stewed tomatoes and macaroni))
(define set2 '(macaroni and cheese))
(intersect? set1 set2)
(define set1 '(stewed tomatoes and1 macaroni1))
(define set2 '(macaroni and cheese))
(intersect? set1 set2)

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) false)
          (else
           (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))
(intersect? set1 set2)

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2)))
          (else
           (intersect (cdr set1) set2)))))
(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else
           (cons (car set1) (union (cdr set1) set2))))))
(union '(stewed tomatoes and macaroni)
       '(macaroni and cheese))

(define diff-set
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (diff-set (cdr set1) set2))
          (else
           (cons (car set1) (diff-set (cdr set1) set2))))))
(diff-set '(stewed tomatoes and macaroni)
          '(macaroni and cheese))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))
(intersectall '((a b c) (c a d e) (e f g h a b)))
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

;; support from Chapter1
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) false)
          ((null? x) false)
          ((null? (cdr x)) false)
          ((null? (cdr (cdr x))) true)
          (else false))))
(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (cadr p)))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else
           (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))
(revrel '((8 a) (pumpkin pie) (got sick)))
(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))
(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else
           (cons (revpair (car rel))
                 (revrel (cdr rel)))))))
(revrel '((1 2) (3 4) (5 6)))

(define (seconds lat)
  (cond ((null? lat) '())
        (else
         (cons (cadar lat)
               (seconds (cdr lat))))))
(seconds '((8 3) (4 8) (7 6) (6 2) (3 4)))
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

