#lang racket

(define/contract (daily-temperatures T)
  (-> (listof exact-integer?) (listof exact-integer?))
  (let ([rt ((lambda (T)
               (define (iterator l rl n)
                 (if (empty? l)
                     (cons rl n)
                     (iterator (cdr l) (cons (car l) rl) (+ n 1))))
               (iterator T '() 0))
             T)])
    '()))