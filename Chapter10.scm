;; -------------------------------------------------------------------
;; ---- helper functions
;; 

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
(define first car)
(define second cadr)
(define third caddr)

;; -------------------------------------------------------------------
;; ---- environment
;;

;; element of environment : entry
(define new-entry
  (lambda (names values)
    (build names values)))
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? (car names) name)
           (car values))
          (else
           (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;; test of entry
(eq?
 (lookup-in-entry 'entree
                 '((appetizer entree beverage)
                   (food tastes good))
                 (lambda (name)
                   (error "No such name: " name)))
 'tastes)

;; environment : table
(define extend-table
  (lambda (new-entry table)
    (cons new-entry table)))
(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else (lookup-in-entry
                 name
                 (car table)
                 (lambda (name)
                   (lookup-in-table name (cdr table) table-f)))))))

;; test of table
(eq?
 (lookup-in-table 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree beverage)
                    (food tastes good)))
                 (lambda (name)
                   (error "No such name: " name)))
 'spaghetti)

;; -------------------------------------------------------------------
;; ---- scheme interpreter

;; 6 types : *const, *quote, *identifier, *lambda, *cond, *application

;; primitive procedures: cons,car,cdr,null?,eq?,atom?,zero?,add1,sub1,number?

;; -----------------
;; expressions -> actions
;;

(define value
  (lambda (e)
    (meaning e (quote ()))))
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))
(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e #t) *const)
          ((eq? e #f) *const)
          ((eq? e (quote cons)) *const)
          ((eq? e (quote car)) *const)
          ((eq? e (quote cdr)) *const)
          ((eq? e (quote null?)) *const)
          ((eq? e (quote eq?)) *const)
          ((eq? e (quote atom?)) *const)
          ((eq? e (quote zero?)) *const)
          ((eq? e (quote add1)) *const)
          ((eq? e (quote sub1)) *const)
          ((eq? e (quote number?)) *const)
          (else *identifier))))
(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) (quote quote))
                  *quote)
                 ((eq? (car e) (quote lambda))
                  *lambda)
                 ((eq? (car e) (quote cond))
                  *cond)
                 (else *application)))
          (else *application))))

;; -----------------
;; 6 types of actions

;; -------------------------------------
;; 1 : *const
(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build (quote primitive) e)))))
;; -------------------------------------

;; -------------------------------------
;; 2 : *quote
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of
  (lambda (exp) (second exp)))
;; -------------------------------------

;; -------------------------------------
;; 3 : *identifier
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ())))) ; when evaluate, will sign an error
;; -------------------------------------

;; -------------------------------------
;; 4 : *lambda
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
; helper functions for accessing different part of *lambda action
(define table-of first)
(define formals-of second)
(define body-of third)
;; -------------------------------------

;; -------------------------------------
;; 5 : *cond
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (cond-lines table)
    (cond
     ((else? (question-of (car cond-lines)))
      (meaning (answer-of (car cond-lines)) table))
     ((meaning (question-of (car cond-lines)) table)
      (meaning (answer-of (car cond-lines)) table))
     (else (evcon (cdr cond-lines) table)))))
(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
          (else #f))))
(define question-of first)
(define answer-of second)

;; test
(define cond-exp '(cond (coffee klatsch) (else party)))
(define cond-env '(((coffee) (#t))
                   ((klatsch party) (5 (6)))))
(eq? (*cond cond-exp cond-env) 5)
;; -------------------------------------

;; -------------------------------------
;; 6 : *application

; apply-my : distinguish it with the outer scheme's apply
(define *application
  (lambda (e table)
    (apply-my (meaning (function-of e) table)
              (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)

;; an application must always determine the meaning of all its arguments
(define evlis
  (lambda (args table)
    (cond ((null? args) (quote ()))
          (else
           (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))


(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply-my
  (lambda (fun vals)
    (cond ((primitive? fun)
           (apply-primitive
            (second fun) vals))
          ((non-primitive? fun)
           (apply-closure
            (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name (quote cons))
           (cons (first vals) (second vals)))
          ((eq? name (quote car))
           (car (first vals)))
          ((eq? name (quote cdr))
           (cdr (first vals)))
          ((eq? name (quote null?))
           (null? (first vals)))
          ((eq? name (quote eq?))
           (eq? (first vals) (second vals)))
          ((eq? name (quote atom?))
           (:atom? (first vals)))
          ((eq? name (quote zero?))
           (zero? (first vals)))
          ((eq? name (quote add1))
           (add1 (first vals)))
          ((eq? name (quote sub1))
           (sub1 (first vals)))
          ((eq? name (quote number?))
           (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) (quote primitive)) #t)
          ((eq? (car x) (quote non-primitive)) #t)
          (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))
;; -------------------------------------