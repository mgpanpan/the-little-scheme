#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))
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
                                    (col newlat (+ sum-odd (car l))
                                         mul-even))))))
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
          (cons product newl)
          )))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
